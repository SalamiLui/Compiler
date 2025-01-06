#pragma once
#include <sstream>

#include "parser.hpp"

class Generator {
public:
    inline explicit Generator(NodeProg &prog)
        : m_prog(std::move(prog)) {
    }

    void gen_term(const NodeTerm *term) {
        struct TermVisitor {
            Generator *gen;

            void operator()(const NodeTermIntLit *term_int_lit) const {
                gen->m_output << "    mov eax, " << term_int_lit->int_lit.value.value() << "\n";
                gen->push("eax");
            }

            void operator()(const NodeTermCharLit *term_char_lit) const {
                gen->m_output << "    mov eax, '" << term_char_lit->char_lit.value.value() << "'\n";
                gen->push("eax");
            }

            void operator()(const NodeTermIdent *term_ident) const {
                const Var &var = gen->searchVar(term_ident->ident.value.value());
                std::stringstream offset;
                offset << "DWORD [esp + " << (gen->m_stack_size - var.stack_loc) * 4 << "]";
                gen->push(offset.str());
            }

            void operator()(const NodeTermParen *term_paren) const {
                gen->gen_expr(term_paren->expr);
            }
        };

        TermVisitor visitor{.gen = this};
        std::visit(visitor, term->var);
    }

    void gen_bin_expr(const NodeBinExpr *bin_expr) {
        struct BinExprVisitor {
            Generator *gen;

            void operator()(const NodeBinExprAdd *bin_expr_add) const {
                gen->gen_expr(bin_expr_add->lhs);
                gen->gen_expr(bin_expr_add->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    add eax, ebx\n";
                gen->push("eax");
            }

            void operator()(const NodeBinExprSub *bin_expr_sub) const {
                gen->gen_expr(bin_expr_sub->lhs);
                gen->gen_expr(bin_expr_sub->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    sub eax, ebx\n";
                gen->push("eax");
            }

            void operator()(const NodeBinExprMulti *bin_expr_multi) const {
                gen->gen_expr(bin_expr_multi->lhs);
                gen->gen_expr(bin_expr_multi->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    mul ebx\n";
                gen->push("eax");
            }

            void operator()(const NodeBinExprDiv *bin_expr_div) const {
                gen->gen_expr(bin_expr_div->lhs);
                gen->gen_expr(bin_expr_div->rhs);
                gen->pop("ecx");
                gen->pop("eax");
                gen->m_output << "    xor edx, edx\n";
                gen->m_output << "    div ecx\n";
                gen->push("eax");
            }
        };
        BinExprVisitor visitor{.gen = this};
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(const NodeExpr *expr) {
        struct ExprVisitor {
            Generator *gen;

            void operator()(const NodeTerm *term) const {
                gen->gen_term(term);
            }


            void operator()(const NodeBinExpr *bin_expr) const {
                gen->gen_bin_expr(bin_expr);
            }
        };
        ExprVisitor visitor{.gen = this};
        std::visit(visitor, expr->var);
    }

    std::string gen_bool_expr(const NodeBoolExpr *bool_expr) {
        struct BoolExprVisitor {
            Generator *gen;

            std::string operator()(const NodeBoolExprEq *bool_expr_eq) const {
                gen->gen_expr(bool_expr_eq->lhs);
                gen->gen_expr(bool_expr_eq->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jne " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprNeq *bool_expr_neq) const {
                gen->gen_expr(bool_expr_neq->lhs);
                gen->gen_expr(bool_expr_neq->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    je " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprGreat *bool_expr_great) const {
                gen->gen_expr(bool_expr_great->lhs);
                gen->gen_expr(bool_expr_great->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jle " << label << "\n";
                return label;
            }


            std::string operator()(const NodeBoolExprLess *bool_expr_less) const {
                gen->gen_expr(bool_expr_less->lhs);
                gen->gen_expr(bool_expr_less->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jge " << label << "\n";
                return label;
            }

            std::string operator()(const NodeExpr *bool_expr) const {
                gen->gen_expr(bool_expr);
                gen->pop("eax");
                gen->m_output << "    cmp eax, 0\n";
                std::string label = gen->label();
                gen->m_output << "    je " << label << "\n";
                return label;
            }
        };
        BoolExprVisitor visitor{.gen = this};
        return std::visit(visitor, bool_expr->var);
    }

    void gen_scope(const NodeScope *scope) {
        size_t const stack_size = m_stack_size;
        for (const NodeStmt &stmt: scope->stmts) {
            gen_stmt(&stmt);
        }
        m_output << "    add esp, " << (m_stack_size - stack_size) * 4 << "\n";
        while (m_stack_size != stack_size) {
            m_vars.pop_back();
            m_stack_size--;
        }
    }

    void gen_if(const NodeStmtIf *stmt_if) { // NOLINT(*-no-recursion)
        auto *gen = this;
        std::string const _else = gen->gen_bool_expr(stmt_if->bool_expr);
        std::string const end_if = gen->label();
        gen->gen_scope(stmt_if->scope);
        gen->m_output << "    jmp " << end_if << "\n";

        if (stmt_if->_else == nullptr) {
            gen->m_output << _else << ":\n";
            gen->m_output << end_if << ":\n";
        } else if (stmt_if->_else->_if == nullptr) {
            gen->m_output << _else << ":\n";
            gen->gen_scope(stmt_if->_else->scope);
            gen->m_output << end_if << ":\n";
        } else {
            gen->m_output << _else << ":\n";
            gen->gen_if(stmt_if->_else->_if);
            gen->m_output << end_if << ":\n";
        }
    }

    void gen_let(const NodeStmtLet* stmt_let) {
        if (isInVars(stmt_let->ident.value.value())) {
            std::cerr << "Identifier " << stmt_let->ident.value.value() << " already exists\n";
            exit(EXIT_FAILURE);
        }
        gen_expr(stmt_let->expr);
        m_vars.push_back(Var{.name = stmt_let->ident.value.value(), .stack_loc = m_stack_size});
    }

    void gen_ident(const NodeStmtIdent* stmt_ident) {
        const Var &var = searchVar(stmt_ident->ident.value.value());
        gen_expr(stmt_ident->expr);
        pop("eax");
        auto const pos = (m_stack_size - var.stack_loc) * 4;
        m_output << "    mov DWORD [esp + " << pos << "], eax\n";
    }


    void gen_stmt(const NodeStmt *stmt) {
        struct StmtVisitor {
            Generator *gen;

            void operator()(const NodeStmtExit *stmt_exit) const {
                gen->gen_expr(stmt_exit->expr);
                gen->pop("edi");
                gen->m_output << "    invoke ExitProcess, edi\n";
            }

            void operator()(const NodeStmtLet *stmt_let) const {
                gen->gen_let(stmt_let);
            }

            void operator()(const NodeStmtIf *stmt_if) const {
                gen->gen_if(stmt_if);
            }

            void operator()(const NodeStmtIdent *stmt_ident) const {
                gen->gen_ident(stmt_ident);
            }

            void operator()(const NodeStmtFor *stmt_for) const {

                bool isLet;
                auto visitor = [&isLet, this](auto&& args) {

                    using T = std::decay_t<decltype(args)>;

                    if constexpr (std::is_same_v<T, NodeStmtLet*>) {
                        gen->gen_let(args);
                        isLet = true;
                    }
                    else if constexpr (std::is_same_v<T, NodeStmtIdent*>) {
                        gen->gen_ident(args);
                        isLet = false;
                    }
                };

                std::visit(visitor, stmt_for->init);

                std::string const start = gen->label();
                gen->m_output << start << ":\n";
                std::string const end = gen->gen_bool_expr(stmt_for->condition);

                gen->gen_scope(stmt_for->scope);
                gen->gen_ident(stmt_for->update);

                gen->m_output << "    jmp " << start << "\n";
                gen->m_output << end << ":\n";

                if (isLet) {
                    gen->m_output << "    add esp, 4\n";
                    gen->m_vars.pop_back();
                    gen->m_stack_size--;
                }

            }

            void operator()(const NodeStmtWhile* stmt_while) const {
                std::string const start = gen->label();
                gen->m_output << start << ":\n";
                std::string const end = gen->gen_bool_expr(stmt_while->condition);
                gen->gen_scope(stmt_while->scope);
                gen->m_output << "    jmp " << start << "\n";
                gen->m_output << end << ":\n";
            }
        };

        StmtVisitor visitor{.gen = this};
        std::visit(visitor, stmt->stmt);
    }

    [[nodiscard]] std::string generate() {
        m_output << "format PE console\n";
        m_output << "entry start\n";
        m_output << "include 'C:\\FASM\\include\\win32a.inc'\n";

        m_output << "section '.idata' import data readable writeable\n";
        m_output << "library kernel32, 'kernel32.dll'\n";
        m_output <<
                "import kernel32, GetStdHandle, 'GetStdHandle', WriteConsoleA, 'WriteConsoleA', ExitProcess, 'ExitProcess'\n";
        m_output << "start:\n";

        for (const NodeStmt &stmt: m_prog.stmts) {
            gen_stmt(&stmt);
        }


        m_output << "    invoke ExitProcess, 0";

        return m_output.str();
    }

private:
    std::string label() {
        n_label++;
        return "label" + std::to_string(n_label);
    }

    void push(const std::string &reg) {
        m_output << "    push " << reg << "\n";
        m_stack_size++;
    }

    void pop(const std::string &reg) {
        m_output << "    pop " << reg << "\n";
        m_stack_size--;
    }

    struct Var {
        std::string name;
        size_t stack_loc;
    };

    [[nodiscard]] bool isInVars(std::string const &name) {
        for (auto const i: m_vars) {
            if (i.name == name) {
                return true;
            }
        }
        return false;
    }

    Var searchVar(std::string const &name) {
        for (auto i: m_vars) {
            if (i.name == name) {
                return i;
            }
        }
        std::cerr << "Error: Variable " << name << " not found\n";
        exit(EXIT_FAILURE);
    }

    int n_label{};
    std::stringstream m_output;
    const NodeProg m_prog;
    size_t m_stack_size{};
    std::vector<Var> m_vars;
};
