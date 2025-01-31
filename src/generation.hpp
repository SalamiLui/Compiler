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

    /**
    * writres to m_output pushing to stack the value of the term, if it's a char or an ident
    * that contains a char then the var m_var_type keeps track of it in case it's needed for
    * implicitly declaration
    */
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


        /**
         * generates the function, if the return type is void throws an exception, else
         * pushes what is int the reg eax (the return value stores in eax)
         */
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

    /**
    * generate the left hand sides and right hand sides of the expretion,
    * makes the operation and finally pushes the result to stack
    */
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

    /**
    * generate the expr depending on if it's a bin expretion or a term,
    * after the generation the expretion type is modfied according to m_var_type
    * to keep track if it's an int_lit expr, a char expr ...
    */
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

    /**
    * generates the left hand side (eax) and the right hand side (ebx) expretions and
    * compares its values
    * a label is returned, if the comparation is false then the program jumps to the location of the
    * returned label
    */
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

            /**
             * if expretion is a 0 then the comparation is false otherwise is true
             */
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

/**
* for each stmt in the scope gen_scope generates the stmt_norm which means that function declarations
* are not permitted in scopes
* m_number_scopes increases by 1 when the scope starts and decreases also by 1 one when the scope
* is finished to keep track of how many layers of scopes are
* the stack size before and after the scope is stored to clean the stack once the scope has finished
* and all the vars in the scope that were stored in m_vars are deleted
*/
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
/**
* generates each part of the if stmt
*/
    void gen_if(const NodeStmtIf *stmt_if) { // NOLINT(*-no-recursion)
        auto *gen = this;
        // it gens the bool expretion and in case it's not true jumps to the else label
        std::string const _else = gen->gen_bool_expr(stmt_if->bool_expr);

        std::string const end_if = gen->label();
        // gens the 'if' stmt scope
        gen->gen_scope(stmt_if->scope);
        // jmps to the end of the 'if' stmt to avoid generate the possible remaining elses
        gen->m_output << "    jmp " << end_if << "\n";

        // if there's no 'else' after the 'if'
        // (is just an if)
        if (stmt_if->_else == nullptr) {
            gen->m_output << _else << ":\n";
            gen->m_output << end_if << ":\n";
        }
        // if there's no 'else if' after the 'if'
        // (is just an if, else)
        // gens the scope of the else
        else if (stmt_if->_else->_if == nullptr) {
            gen->m_output << _else << ":\n";
            gen->gen_scope(stmt_if->_else->scope);
            gen->m_output << end_if << ":\n";
        }
        // is and if, else if
        // it gens the 'if' stmt
        else {
            gen->m_output << _else << ":\n";
            gen->gen_if(stmt_if->_else->_if);
            gen->m_output << end_if << ":\n";
        }
    }

    /**
    * m_var_type is started to typeInt to avoid using its previous value,
    * (when called gen_expr m_var_type can change depending on the terms generated in the expretion)
    * depending on if the var is explicitly or implicitly declared the type recives a value
    * the var is stored in m_vars if the ident isn't already in use
    */
    void gen_let(const NodeStmtLet* stmt_let) {
        m_var_type = typeInt;
        gen_expr(stmt_let->expr);
        TokenType type;
        if (stmt_let->type.has_value()) { // it's explicit
            type = stmt_let->type.value();
        }
        else { // it's implicit and m_var_type has to be used to give a type
            type = m_var_type;
        }
        m_vars.newVar(stmt_let->ident.value.value(), type);
    }

    /**
    * gens the expretion, if there is an explicit declaration the type of the var is changed
    * then the location of the var is searched and the value in that location is overwritten
    * with the new expretion's value
    */
    void gen_ident(const NodeStmtIdent* stmt_ident) {

        gen_expr(stmt_ident->expr);

        if (stmt_ident->type.has_value()) { // it has an explicit declaration
            const auto var = m_vars.getVar(stmt_ident->ident.value.value()).value();
            var->type = stmt_ident->type.value();
        }

        m_output << m_vars.pop("eax");
        // the location of the var in the stack
        auto const pos = m_vars.getVarsSize() - m_vars.getVarLoc(stmt_ident->ident.value.value());
        m_output << "    mov DWORD [esp + " << pos * 4 << "], eax\n";
    }

    /**
    * prints a single char which is already in stack
    * once printed the stack is cleaned
    */
    void print_char(NodeStmtPrint const * stmt_print) {
        auto *gen = this;
        gen->m_output << "    invoke GetStdHandle, STD_OUTPUT_HANDLE\n";
        gen->m_output << "    lea ebx, [esp]\n";
        gen->m_output << "    invoke WriteConsoleA, eax, ebx, 4, 0 ,0\n";
        gen->m_output << gen->m_vars.pop();
    }

    /**
    * prints a single int which is already in stack
    * first each digit in the int is converted to a char and pushed to stack in execution time
    * by dividing the quotient by 10 and the remainder is pushed to stack with
    * its correspondent ascii value, the process repeats until the whole integer is
    * in the stack (the quotient will be 0)
    * then the int is printed and the stack cleaned
    *
    * TODO print negative ints
    */
    void print_int(NodeStmtPrint const * stmt_print) {
        auto* gen = this;
        std::string const start = gen->label();
        std::string const end = gen->label();


        // convertion from int to chars
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

/**
* gens each expretion (argument) in the function
* m_vars starts the functions (checks if the function exists, number of arguments ... )
* the function is called and finally when the function ends the stack is cleaned
*/
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

    /**
    * m_all_path_return is initialized to false to avoid using its previous result,
    * m_vars creates the new function unless it already exist and prepares everything
    * (creates a new stack vector in  m_vars.m_varsStack to keep track of the new vars, etc ),
    * the label 'end' is used to avoid execute the function by accident unless it's called,
    * once the function has finished m_vars pops the remains of the function
    */
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

    // used inside scopes to avoid gen stmts that are not permited inside scopes
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

                // isLet is used to clean the let var from stack in case it exists
                bool isLet;
                // gens the init depending on if it's a let stmt or a ident stmt
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
                // gens the condition, if its false jumps to the end label
                // otherwise executes the scope
                std::string const end = gen->gen_bool_expr(stmt_for->condition);

                gen->gen_scope(stmt_for->scope);
                gen->gen_ident(stmt_for->update);

                // repeats the process until the condition is false
                gen->m_output << "    jmp " << start << "\n";
                gen->m_output << end << ":\n";

                // if the init is a let stmt the var is popped from stack
                if (isLet) {
                    gen->m_vars.deleteVars(1);
                    gen->m_output << gen->m_vars.pop();

                }

            }

            void operator()(const NodeStmtWhile* stmt_while) const {
                std::string const start = gen->label();
                gen->m_output << start << ":\n";
                // gens the condition, if its false jumps to the end label
                // otherwise executes the scope
                std::string const end = gen->gen_bool_expr(stmt_while->condition);
                gen->gen_scope(stmt_while->scope);
                // repeats the process until the condition is false
                gen->m_output << "    jmp " << start << "\n";
                gen->m_output << end << ":\n";
            }

            /**
             * starts m_var_type to avoid using its previous value
             * (m_var_type may be modified when calling gen_expr)
             * gens the expretion to print and according to the type
             * calls print int or print char
             */
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

            /**
             * gens the expretion to return (if it has one)
             * stores the expr in the eax reg (so it can be later used without having to modify the stack)
             * if the return is in the 'main' fun (the return stmt is global) the program exits with the value of the expr
             * if the return is in a function cleans the stack and uses 'ret' to avoid executing the rest of the function
             */
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

                    // else just clean the stack and use 'ret' without storing a value in eax (without returning any value)
                    size_t const times = gen->m_vars.getVarsSize() - (gen->m_vars.m_funs_args_size.back() + 1);
                    gen->m_output << "    add esp, " << times * 4<< "\n";
                    gen->m_output << "    ret\n";
                    return;
                }
                // if it's a non void and doesn't have an expretion to return
                if (gen->m_vars.m_currentFun != nullptr && stmt_return->expr == nullptr) {
                    std::cerr << "Return value is missing in function " << gen->m_vars.m_currentFun->name << std::endl;
                    exit(EXIT_FAILURE);
                }

                // if return is in 'main' and doesn't have an expretion
                if (gen->m_number_scopes == 0 && stmt_return->expr == nullptr) {
                    std::cerr << "Return value is missing for global return" << std::endl;
                    exit(EXIT_FAILURE);
                }

                // generating the expretion and storing the value in eax for using it later
                gen->gen_expr(stmt_return->expr);
                gen->m_output << gen->m_vars.pop("eax");

                // if return is in 'main' and does have an expretion
                if (gen->m_vars.m_funs_args_size.size() == 1) {
                    gen->m_output << "    invoke ExitProcess, eax\n";
                    return;
                }

                // otherwise is inside a function
                size_t const times = gen->m_vars.getVarsSize() - (gen->m_vars.m_funs_args_size.back() + 1);
                gen->m_output << "    add esp, " << times * 4<< "\n";
                gen->m_output << "    ret\n";

            }

            /**
             * if called when inside a scope the exception below will be executed
             * if called by gen_stmt this function will never be used
             */
            void operator()(const NodeStmtFun* stmt_fun) const {
                std::cerr << "Unable to declare function inside scope\n";
                exit(EXIT_FAILURE);
            }
        };

        StmtVisitor visitor{.gen = this};
        std::visit(visitor, stmt->stmt);
    }


    /**
    * not called when inside a scope
    * it can gen stmts like functions declarations
    *
    */
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
        // the format and imports to generate the asm file
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

        // if there's no exit / return automatically exit with 0
        m_output << "    invoke ExitProcess, 0";

        return m_output.str();
    }

private:
    /**
    * generates a new label each time it's called and return its name
    *
    * example:
    *  string l1 = label()
    *  cout<<l1; // label1
    *  string l2 = label();
    *  cout<<l2; // label2
    *
    * used to generate names for jump instructions
    */
    std::string label() {
        n_label++;
        return "label" + std::to_string(n_label);
    }
    int n_label{}; // keeps record of the number of labels

    // the generated code in asm
    std::stringstream m_output;
    // the parse tree
    const NodeProg m_prog;


    //keeps track of vars, functions and its elements (args, sizes, locations ...)
    Vars m_vars;

    // keeps record of how many layers of scopes are at the moment
    size_t m_number_scopes{};

    // keeps record if the function that is generating at the moment returns always a value
    // TODO delete this shit and figure something better
    bool m_all_path_return{};

    /**
    * used to determine the type of a var when generating an expretion
    *
    * it only changes its value when the type is different to int
    * example:
    *  expretion = 1 + 'h'
    *  the expretion contains a term with a char 'h' so m_var_type will be equal to typeChar
    *  expretion = 12 + 5 * 1
    *  the expretion doesn't contain a char thus m_var_type doesn't change
    *
    * before using m_var_type make shure to initialize to typeInt to avoid using previous value
    *
    * TODO delete this shit too and figure smthng better
    */
    TokenType m_var_type;
};
