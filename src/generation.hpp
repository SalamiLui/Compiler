#pragma once
#include <sstream>
#include <unordered_map>

#include "parser.hpp"

class Generator {
public:

    inline explicit Generator(NodeProg& prog)
        : m_prog(std::move(prog))
    {
    }

    void gen_term(const NodeTerm* term) {
        struct TermVisitor {
            Generator* gen;

            void operator()(const NodeTermIntLit* term_int_lit) const {
                gen->m_output << "    mov eax, " << term_int_lit->int_lit.value.value() << "\n";
                gen->push("eax");
            }
            void operator()(const NodeTermIdent* term_ident) const{
                if (!gen->m_vars.contains(term_ident->ident.value.value())) {
                    std::cerr << "Error: Variable " << term_ident->ident.value.value() << " not found\n";
                    exit(EXIT_FAILURE);
                }
                const Var& var = gen->m_vars.at(term_ident->ident.value.value());
                std::stringstream offset;
                offset << "DWORD [esp + " << (gen->m_stack_size - var.stack_loc ) * 4 << "]\n";
                gen->push(offset.str());
            }
            void operator()(const NodeTermParen* term_paren) const {
                gen->gen_expr(term_paren->expr);
            }
        };

        TermVisitor visitor{.gen = this};
        std::visit(visitor, term->var);
    }

    void gen_bin_expr(const NodeBinExpr* bin_expr) {
        struct BinExprVisitor {
            Generator* gen;

            void operator()(const NodeBinExprAdd* bin_expr_add) const {
                gen->gen_expr(bin_expr_add->lhs);
                gen->gen_expr(bin_expr_add->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    add eax, ebx\n";
                gen->push("eax");
            }
            void operator()(const NodeBinExprSub* bin_expr_sub) const{
                gen->gen_expr(bin_expr_sub->lhs);
                gen->gen_expr(bin_expr_sub->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    sub eax, ebx\n";
                gen->push("eax");
            }
            void operator()(const NodeBinExprMulti* bin_expr_multi) const{
                gen->gen_expr(bin_expr_multi->lhs);
                gen->gen_expr(bin_expr_multi->rhs);
                gen->pop("ebx");
                gen->pop("eax");
                gen->m_output << "    mul ebx\n";
                gen->push("eax");
            }
            void operator()(const NodeBinExprDiv* bin_expr_div) const{
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

    void gen_expr(const NodeExpr* expr) {
        struct ExprVisitor {
            Generator* gen;

            void operator()(const NodeTerm* term) const {
                gen->gen_term(term);
            }


            void operator()(const NodeBinExpr* bin_expr) const {
                gen->gen_bin_expr(bin_expr);
            }
        };
        ExprVisitor visitor{.gen = this};
        std::visit(visitor, expr->var);
    }

    void gen_stmt(const NodeStmt* stmt) {
        struct StmtVisitor {
            Generator* gen;

            void operator()(const NodeStmtExit* stmt_exit) const {
                gen->gen_expr(stmt_exit->expr);
                gen->pop("edi");
                gen->m_output << "    invoke ExitProcess, edi\n";
            }
            void operator()(const NodeStmtLet* stmt_let) const {
                if (gen->m_vars.contains(stmt_let->ident.value.value())) {
                    std::cerr << "Identifier " << stmt_let->ident.value.value() << " already exists\n";
                    exit(EXIT_FAILURE);
                }
                gen->gen_expr(stmt_let->expr);
                gen->m_vars.insert({stmt_let->ident.value.value(), Var {.stack_loc = gen->m_stack_size}});
            }
        };

        StmtVisitor visitor{.gen = this};
        std::visit(visitor, stmt->stmt);
    }

    [[nodiscard]] std::string generate()
    {
        m_output << "format PE console\n";
        m_output << "entry start\n";
        m_output << "include 'C:\\FASM\\include\\win32a.inc'\n";

        m_output << "section '.idata' import data readable writeable\n";
        m_output << "library kernel32, 'kernel32.dll'\n";
        m_output << "import kernel32, \\ \n";
        m_output << "       ExitProcess, 'ExitProcess'\n";
        m_output << "section '.code' code readable executable\n";
        m_output << "start:\n";

        for (const NodeStmt& stmt : m_prog.stmts) {
            gen_stmt(&stmt);
        }


        m_output << "    invoke ExitProcess, 0";

        return m_output.str();
    }



private:

    void push(const std::string& reg) {
        m_output << "    push " << reg << "\n";
        m_stack_size++;
    }

    void pop(const std::string& reg) {
        m_output << "    pop " << reg << "\n";
        m_stack_size--;
    }

    struct Var {
        size_t stack_loc;
    };

    std::stringstream m_output;
    const NodeProg m_prog;
    size_t m_stack_size{};
    std::pmr::unordered_map<std::string, Var> m_vars;
};

