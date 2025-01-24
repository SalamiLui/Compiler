#pragma once
#include <sstream>
#include "Vars.hpp"


class Generator {
public:
    inline explicit Generator(NodeProg &prog)
        : m_prog(std::move(prog))
    {
        m_var_type = int_lit;
    }

    void gen_term(const NodeTerm *term) {
        struct TermVisitor {
            Generator *gen;

            void operator()(const NodeTermIntLit *term_int_lit) const {
                gen->m_output << "    mov eax, " << term_int_lit->int_lit.value.value() << "\n";
                gen->m_output << gen->m_vars.push("eax");
            }

            void operator()(const NodeTermCharLit *term_char_lit) const {
                gen->m_output << "    mov eax, '" << term_char_lit->char_lit.value.value() << "'\n";
                gen->m_output << gen->m_vars.push("eax");
                gen->m_var_type = typeChar;
            }

            void operator()(const NodeTermIdent *term_ident) const {
                std::stringstream offset;
                offset << "DWORD [esp + " << gen->m_vars.getVarPosBytes(term_ident->ident.value.value()) << "]";
                gen->m_output << gen->m_vars.push(offset.str());
                const auto type = gen->m_vars.getVar(term_ident->ident.value.value()).value()->type;
                if (type == typeChar) {
                    gen->m_var_type = type;
                }

            }



            void operator()(const NodeTermFunIdent* term_fun) const {
                gen->gen_fun_ident(term_fun);

                if (gen->m_vars.getFun(term_fun->name).value()->returnType == typeChar) {
                    gen->m_var_type = typeChar;
                }
                else if (gen->m_vars.getFun(term_fun->name).value()->returnType == typeVoid) {
                    std::cerr << "Function " << term_fun->name << " returns void, unable to gen expretion, try changing return type\n";
                }

                gen->m_output << gen->m_vars.push("eax");
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
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    add eax, ebx\n";
                gen->m_output << gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprSub *bin_expr_sub) const {
                gen->gen_expr(bin_expr_sub->lhs);
                gen->gen_expr(bin_expr_sub->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    sub eax, ebx\n";
                gen->m_output << gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprMulti *bin_expr_multi) const {
                gen->gen_expr(bin_expr_multi->lhs);
                gen->gen_expr(bin_expr_multi->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    mul ebx\n";
                gen->m_output << gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprDiv *bin_expr_div) const {
                gen->gen_expr(bin_expr_div->lhs);
                gen->gen_expr(bin_expr_div->rhs);
                gen->m_output << gen->m_vars.pop("ecx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    xor edx, edx\n";
                gen->m_output << "    div ecx\n";
                gen->m_output << gen->m_vars.push("eax");
            }
        };
        BinExprVisitor visitor{.gen = this};
        std::visit(visitor, bin_expr->var);
    }

    void gen_expr(NodeExpr *expr) {
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
        expr->type = m_var_type;
    }

    std::string gen_bool_expr(const NodeBoolExpr *bool_expr) {
        struct BoolExprVisitor {
            Generator *gen;

            std::string operator()(const NodeBoolExprEq *bool_expr_eq) const {
                gen->gen_expr(bool_expr_eq->lhs);
                gen->gen_expr(bool_expr_eq->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jne " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprNeq *bool_expr_neq) const {
                gen->gen_expr(bool_expr_neq->lhs);
                gen->gen_expr(bool_expr_neq->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    je " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprGreat *bool_expr_great) const {
                gen->gen_expr(bool_expr_great->lhs);
                gen->gen_expr(bool_expr_great->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jle " << label << "\n";
                return label;
            }


            std::string operator()(const NodeBoolExprLess *bool_expr_less) const {
                gen->gen_expr(bool_expr_less->lhs);
                gen->gen_expr(bool_expr_less->rhs);
                gen->m_output << gen->m_vars.pop("ebx");
                gen->m_output << gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jge " << label << "\n";
                return label;
            }

            std::string operator()(NodeExpr *bool_expr) const {
                gen->gen_expr(bool_expr);
                gen->m_output << gen->m_vars.pop("eax");
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
        m_number_scopes++;
        size_t const stack_size_before = m_vars.getVarsSize();
        for (const NodeStmt &stmt: scope->stmts) {
            gen_stmt_norm(&stmt);
        }
        size_t const times = (m_vars.getVarsSize() - stack_size_before);
        m_vars.deleteVars(times);
        m_output << m_vars.pop(times);

        if (m_number_scopes == 1 && m_vars.m_currentFun != nullptr) { // if the scope comes from a fun
            // if not all paths return and function is not a void
            if (m_vars.m_currentFun->returnType != typeVoid && !m_all_path_return) {
                std::cerr << "Not all paths return a value in function " << m_vars.m_currentFun->name << "\n";
                exit(EXIT_FAILURE);
            }
        }
        m_number_scopes--;
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
        m_var_type = typeInt;
        gen_expr(stmt_let->expr);
        TokenType type;
        if (stmt_let->type.has_value()) {
            type = stmt_let->type.value();
        }
        else {
            type = m_var_type;
        }
        m_vars.newVar(stmt_let->ident.value.value(), type);
    }

    void gen_ident(const NodeStmtIdent* stmt_ident) {

        gen_expr(stmt_ident->expr);

        if (stmt_ident->type.has_value()) {
            const auto var = m_vars.getVar(stmt_ident->ident.value.value()).value();
            var->type = stmt_ident->type.value();
        }

        m_output << m_vars.pop("eax");
        auto const pos = m_vars.getVarsSize() - m_vars.getVarLoc(stmt_ident->ident.value.value());
        m_output << "    mov DWORD [esp + " << pos * 4 << "], eax\n";
    }

    void print_char(NodeStmtPrint const * stmt_print) {
        auto *gen = this;
        gen->m_output << "    invoke GetStdHandle, STD_OUTPUT_HANDLE\n";
        gen->m_output << "    lea ebx, [esp]\n";
        gen->m_output << "    invoke WriteConsoleA, eax, ebx, 4, 0 ,0\n";
        gen->m_output << gen->m_vars.pop();
    }

    void print_int(NodeStmtPrint const * stmt_print) {
        auto* gen = this;
        std::string const start = gen->label();
        std::string const end = gen->label();


        // convertion from int to char
        gen->m_output << gen->m_vars.pop("eax");
        gen->m_output << "    xor ecx, ecx\n";
        gen->m_output << "    mov ebx, 10\n";
        gen->m_output << start << ":\n";
        gen->m_output << "    xor edx, edx\n";
        gen->m_output << "    div ebx\n";
        gen->m_output << "    add edx, 48\n";
        gen->m_output << "    push edx\n";
        gen->m_output << "    add ecx, 4\n";
        gen->m_output << "    cmp eax, 0\n";
        gen->m_output << "    je " << end << "\n";
        gen->m_output << "    jmp " << start << "\n";
        gen->m_output << end << ":\n";

        // print the string (int)
        gen->m_output << "    invoke GetStdHandle, STD_OUTPUT_HANDLE\n";
        gen->m_output << "    lea ebx, [esp]\n";
        gen->m_output << "    push ecx\n";
        gen->m_output << "    invoke WriteConsoleA, eax, ebx, ecx, 0 ,0\n";
        gen->m_output << "    pop ecx\n";

        // restore the stack size
        gen->m_output << "    add esp, ecx\n";
    }


    template <typename T>
    void gen_fun_ident(T* fun_ident) {

        for(size_t i = 0; i < fun_ident->args.size(); i++ ) {
            gen_expr(fun_ident->args[i]);
        }
        m_var_type = typeInt;
        m_vars.startFun(fun_ident);

        m_output << "    call fun_" << fun_ident->name << "\n";

        m_output << m_vars.pop(fun_ident->args.size());
    }

    void gen_fun (const NodeStmtFun* stmt_fun) {

        const auto gen = this;
        gen->m_all_path_return = false;
        gen->m_vars.newFun(stmt_fun);

        std::string const end = gen->label();
        gen->m_output << "    jmp " << end << "\n";
        gen->m_output << "fun_" << stmt_fun->name << ":\n";
        gen->gen_scope(stmt_fun->scope);
        gen->m_output << "    ret\n";
        gen->m_output << end << ":\n";

        gen->m_vars.m_varsStack.pop_back();
        gen->m_vars.m_funs_args_size.pop_back();

        gen->m_vars.m_currentFun = nullptr;
    }


    void gen_stmt_norm(const NodeStmt*stmt) {
        struct StmtVisitor {
            Generator* gen;

            void operator()(const NodeStmtExit *stmt_exit) const {
                gen->gen_expr(stmt_exit->expr);
                gen->m_output << gen->m_vars.pop("edi");
                gen->m_output << "    invoke ExitProcess, edi\n";
            }

            void operator()(const NodeStmtLet *stmt_let) const {
                gen->m_var_type = int_lit;
                gen->gen_let(stmt_let);
            }

            void operator()(const NodeStmtIf *stmt_if) const {
                gen->gen_if(stmt_if);
            }

            void operator()(const NodeStmtIdent *stmt_ident) const {
                gen->m_var_type = int_lit;
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
                    gen->m_vars.deleteVars(1);
                    gen->m_output << gen->m_vars.pop();

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

            void operator()(const NodeStmtPrint* stmt_print) const {

                gen->m_var_type = typeInt;
                gen->gen_expr(stmt_print->expr);
                stmt_print->expr->type = gen->m_var_type;

                if (stmt_print->expr->type == typeChar) {
                    gen->print_char(stmt_print);
                }
                else {
                    gen->print_int(stmt_print);
                }
            }

            void operator()(const NodeStmtFunIdent* stmt_fun_ident) const{
                gen->gen_fun_ident(stmt_fun_ident);
            }

            void operator()(const NodeStmtReturn* stmt_return) const {
                // if the return is in the first scope mark like all paths return a value
                if (gen->m_number_scopes == 1) {
                    gen->m_all_path_return = true;
                }

                if (gen->m_vars.m_currentFun != nullptr && gen->m_vars.m_currentFun->returnType == typeVoid) {
                    // if trying to return a value with a void function
                    if (stmt_return->expr != nullptr) {
                        std::cerr << "Return type is void, can't return a value in function " << gen->m_vars.m_currentFun->name << std::endl;
                        exit(EXIT_FAILURE);
                    }

                    size_t const times = gen->m_vars.getVarsSize() - (gen->m_vars.m_funs_args_size.back() + 1);
                    gen->m_output << "    add esp, " << times * 4<< "\n";
                    gen->m_output << "    ret\n";
                    return;
                }
                // if it's not a void and doesn't have an expretion to return
                if (gen->m_vars.m_currentFun != nullptr && stmt_return->expr == nullptr) {
                    std::cerr << "Return value is missing in function " << gen->m_vars.m_currentFun->name << std::endl;
                    exit(EXIT_FAILURE);
                }

                // if return is in 'main' and doesn't have an expretion
                if (gen->m_number_scopes == 0 && stmt_return->expr == nullptr) {
                    std::cerr << "Return value is missing for global return" << std::endl;
                    exit(EXIT_FAILURE);
                }

                gen->gen_expr(stmt_return->expr);
                gen->m_output << gen->m_vars.pop("eax");
                if (gen->m_vars.m_funs_args_size.size() == 1) {
                    gen->m_output << "    invoke ExitProcess, eax\n";
                    return;
                }
                size_t const times = gen->m_vars.getVarsSize() - (gen->m_vars.m_funs_args_size.back() + 1);
                gen->m_output << "    add esp, " << times * 4<< "\n";
                gen->m_output << "    ret\n";

            }

            void operator()(const NodeStmtFun* stmt_fun) const {
                std::cerr << "Unable to declare function inside scope\n";
                exit(EXIT_FAILURE);
            }
        };

        StmtVisitor visitor{.gen = this};
        std::visit(visitor, stmt->stmt);
    }


    void gen_stmt(const NodeStmt *stmt) {

        auto visitor = [&](auto&& args) {

            using T = std::decay_t<decltype(args)>;

            if constexpr (std::is_same_v<T, NodeStmtFun*>) {
                gen_fun(args);
            }
            else  {
                gen_stmt_norm(stmt);
            }
        };

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

    int n_label{};
    std::stringstream m_output;
    const NodeProg m_prog;

    Vars m_vars;
    size_t m_number_scopes{};
    bool m_all_path_return{};

    TokenType m_var_type;
};
