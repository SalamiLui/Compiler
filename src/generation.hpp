#pragma once
#include <sstream>
#include "Vars.hpp"


class Generator {
public:

    inline explicit Generator(NodeProg &prog)
        : m_prog(std::move(prog)), m_vars(m_output) {
    }

    /**
 * @brief Generates assembly code for a term and pushes it onto the stack.
 *
 * This function generates assembly instructions for a given term, whether it is an integer literal,
 * a character literal, or an identifier. The generated assembly code pushes the term onto the stack.
 * If the term is a character or an identifier containing a character, the `term_type` is updated to track
 * the type of the term, which may be needed for implicit declarations.
 *
 * @param term Pointer to the term (`NodeTerm`) whose value will be processed and pushed to the stack.
 * @return TokenType The type of the term (either `typeInt` or `typeChar`).
 */
    TokenType gen_term(const NodeTerm *term) {
        struct TermVisitor {
            Generator *gen{};
            TokenType term_type{typeInt};


            void operator()(const NodeTermIntLit *term_int_lit) const {
                gen->m_output << "    mov eax, " << term_int_lit->int_lit.value.value() << "\n";
                gen->m_vars.push("eax");
            }

            void operator()(const NodeTermCharLit *term_char_lit)  {
                gen->m_output << "    mov eax, '" << term_char_lit->char_lit.value.value() << "'\n";
                gen->m_vars.push("eax");
                term_type = typeChar;
            }

            void operator()(const NodeTermIdent *term_ident)  {
                std::stringstream offset;
                offset << "DWORD [esp + " << gen->m_vars.getVarPosBytes(term_ident->ident.value.value()) << "]";
                gen->m_vars.push(offset.str());
                const auto type = gen->m_vars.getVar(term_ident->ident.value.value()).value()->type;
                if (type == typeChar) {
                    term_type = typeChar;
                }

            }


        /**
         * generates the function, if the return type is void throws an exception, else
         * pushes what is int the reg eax (the return value stores in eax)
         */
            void operator()(const NodeTermFunIdent* term_fun)  {
                gen->gen_fun_ident(term_fun);

                if (gen->m_vars.getFun(term_fun->name).value()->returnType == typeChar) {
                    term_type = typeChar;
                }
                else if (gen->m_vars.getFun(term_fun->name).value()->returnType == typeVoid) {
                    std::cerr << "Function " << term_fun->name << " returns void, unable to gen expretion, try changing return type\n";
                }

                gen->m_vars.push("eax");
            }

            void operator()(const NodeTermArrayIndex* term_array_index) {

                // compute the index in bytes
                gen->gen_expr(term_array_index->index);
                gen->m_vars.pop("eax");
                gen->m_output << "    mov ecx, 4\n";
                gen->m_output << "    mul ecx\n";

                // add to index the array's position to get the item's position
                size_t const pos = gen->m_vars.getVarPosBytes(term_array_index->ident.value.value());
                gen->m_output << "    add eax, " << pos << "\n";

                if (gen->m_vars.getVar(term_array_index->ident.value.value()).value()->type == TokenType::typeChar) {
                    term_type = typeChar;
                };

                // push the value
                gen->m_vars.push("DWORD [esp + eax]");
            }


            void operator()(const NodeTermParen *term_paren) {
                term_type = gen->gen_expr(term_paren->expr);
            }
        };

        TermVisitor visitor{.gen = this};
        std::visit(visitor, term->var);
        return visitor.term_type;
    }

    /**
 * @brief Generates assembly code for a binary expression and pushes the result to the stack.
 *
 * This function generates the left-hand side and right-hand side expressions, performs the binary
 * operation (addition, subtraction, multiplication, or division), and then pushes the result onto the stack.
 * It handles the `eax` and `ebx` registers during the operation and determines the resulting type based
 * on the operand types. If either operand is of type `char`, the result will be of type `char`.
 *
 * @param bin_expr Pointer to the binary expression (`NodeBinExpr`) to be processed.
 * @return TokenType The type of the result of the binary expression, either `typeInt` or `typeChar`.
 */
    TokenType gen_bin_expr(const NodeBinExpr *bin_expr) {

        struct BinExprVisitor {
            Generator *gen;
            TokenType lhs_type, rhs_type;

            void operator()(const NodeBinExprAdd *bin_expr_add) {
                lhs_type = gen->gen_expr(bin_expr_add->lhs);
                rhs_type = gen->gen_expr(bin_expr_add->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    add eax, ebx\n";
                gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprSub *bin_expr_sub) {
                lhs_type = gen->gen_expr(bin_expr_sub->lhs);
                rhs_type = gen->gen_expr(bin_expr_sub->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    sub eax, ebx\n";
                gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprMulti *bin_expr_multi) {
                lhs_type = gen->gen_expr(bin_expr_multi->lhs);
                rhs_type = gen->gen_expr(bin_expr_multi->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    mul ebx\n";
                gen->m_vars.push("eax");
            }

            void operator()(const NodeBinExprDiv *bin_expr_div) {
                lhs_type = gen->gen_expr(bin_expr_div->lhs);
                rhs_type = gen->gen_expr(bin_expr_div->rhs);
                gen->m_vars.pop("ecx");
                gen->m_vars.pop("eax");
                gen->m_output << "    xor edx, edx\n";
                gen->m_output << "    div ecx\n";
                gen->m_vars.push("eax");
            }
        };
        BinExprVisitor visitor{.gen = this};
        std::visit(visitor, bin_expr->var);

        return (visitor.lhs_type == typeChar || visitor.rhs_type == typeChar) ? typeChar : typeInt;
    }

    /**
 * @brief Generates the expression based on whether it's a binary expression or a term.
 *
 * This function determines the type of the expression (`NodeExpr`), processes it accordingly,
 * and modifies the `expr->type` field to track whether the expression is an integer literal,
 * a character expression, or other types of expressions. It uses a visitor pattern to handle
 * both terms and binary expressions.
 *
 * @param expr Pointer to the expression node (`NodeExpr`) that will be processed.
 * @return TokenType The type of the generated expression, either `typeInt`, `typeChar`, etc.
 */
    TokenType gen_expr(NodeExpr *expr) {
        struct ExprVisitor {
            Generator *gen;
            TokenType var_type;

            void operator()(const NodeTerm *term) {
                var_type = gen->gen_term(term);
            }


            void operator()(const NodeBinExpr *bin_expr) {
                var_type = gen->gen_bin_expr(bin_expr);
            }
        };
        ExprVisitor visitor{.gen = this};
        std::visit(visitor, expr->var);
        expr->type = visitor.var_type;
        return visitor.var_type;
    }

    /**
 * @brief Generates assembly code for a boolean expression and returns a label for conditional jump.
 *
 * This function processes different types of boolean expressions (equality, inequality, greater-than,
 * less-than, or simple boolean expression) and generates the corresponding assembly code to compare
 * their values. A label is returned, and if the comparison is false, the program will jump to the label's location.
 * The comparison is performed using registers `eax` (for the left-hand side) and `ebx` (for the right-hand side).
 *
 * @param bool_expr Pointer to the boolean expression node (`NodeBoolExpr`) to be processed.
 * @return std::string A label for the conditional jump; if the comparison is false, the program jumps to this label.
 */
    std::string gen_bool_expr(const NodeBoolExpr *bool_expr) {
        struct BoolExprVisitor {
            Generator *gen;

            std::string operator()(const NodeBoolExprEq *bool_expr_eq) const {
                gen->gen_expr(bool_expr_eq->lhs);
                gen->gen_expr(bool_expr_eq->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jne " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprNeq *bool_expr_neq) const {
                gen->gen_expr(bool_expr_neq->lhs);
                gen->gen_expr(bool_expr_neq->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    je " << label << "\n";
                return label;
            }

            std::string operator()(const NodeBoolExprGreat *bool_expr_great) const {
                gen->gen_expr(bool_expr_great->lhs);
                gen->gen_expr(bool_expr_great->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
                gen->m_output << "    cmp eax, ebx\n";
                std::string label = gen->label();
                gen->m_output << "    jle " << label << "\n";
                return label;
            }


            std::string operator()(const NodeBoolExprLess *bool_expr_less) const {
                gen->gen_expr(bool_expr_less->lhs);
                gen->gen_expr(bool_expr_less->rhs);
                gen->m_vars.pop("ebx");
                gen->m_vars.pop("eax");
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
                gen->m_vars.pop("eax");
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
     * @brief Generates code for all statements within a scope, managing stack and variable cleanup.
     *
     * This function processes all statements within a given scope, ensuring that function declarations
     * are not permitted inside scopes. It manages the scope depth by adjusting the number of active scopes,
     * and it tracks the stack size before and after processing the statements to ensure proper stack cleanup.
     * After processing the scope, all variables within the scope are deleted from the variable table.
     * Additionally, if the scope belongs to a function, it checks if all paths return a value,
     * issuing an error if not all paths are covered and the function is not of type `void`.
     *
     * @param scope Pointer to the scope node (`NodeScope`) whose statements need to be processed.
     */
    void gen_scope(const NodeScope *scope) {
        m_number_scopes++;
        size_t const stack_size_before = m_vars.getVarsSize();
        for (const NodeStmt &stmt: scope->stmts) {
            gen_stmt_norm(&stmt);
        }
        size_t const times = (m_vars.getVarsSize() - stack_size_before);
        m_vars.deleteVars(times);

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
     * @brief Generates assembly code for an `if` statement with optional `else` and `else if` clauses.
     *
     * This function generates the assembly code for an `if` statement by processing the boolean expression
     * and branching based on its evaluation. If the boolean expression is false, it jumps to the `else` label.
     * The function also handles generating the code for the associated `if` block, as well as any `else` or `else if`
     * blocks if present. Additionally, it ensures proper branching to avoid generating unnecessary code after the `if` block.
     *
     * @param stmt_if Pointer to the `NodeStmtIf` node representing the `if` statement.
     * @note This function handles nested `if`, `else if`, and `else` clauses recursively.
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
 * @brief Generates the code for a variable declaration and initialization.
 *
 * This function handles the generation of assembly code for a variable declaration in the form of a `let` statement.
 * It generates the expression on the right-hand side, assigns the appropriate type (explicit or implicit),
 * and stores the variable in the variable map if it's not already in use. If the type is explicitly declared,
 * it overrides the type inferred from the expression.
 *
 * @param stmt_let Pointer to the `NodeStmtLet` node representing the `let` statement, which includes the
 *                 variable identifier, its type, and the expression used to initialize it.
 */
    void gen_let(const NodeStmtLet* stmt_let) {
        TokenType type = gen_expr(stmt_let->expr);
        if (stmt_let->type.has_value()) { // it's explicit
            type = stmt_let->type.value();
        }

        m_vars.newVar(stmt_let->ident.value.value(), type);
    }

    /**
 * @brief Generates the code to update the value of a variable in memory.
 *
 * This function generates the code to evaluate an expression, updates the type of a variable if it is explicitly declared,
 * and overwrites the existing value of the variable with the result of the evaluated expression.
 * It first generates the expression's value, then updates the variable's type (if applicable),
 * and finally writes the result back to the memory location where the variable is stored.
 *
 * @param stmt_ident Pointer to the `NodeStmtIdent` node representing the statement where a variable's value
 *                  is updated with a new expression. It contains the variable identifier, the expression,
 *                  and an optional explicit type declaration.
 */
    void gen_ident(const NodeStmtIdent* stmt_ident) {

        gen_expr(stmt_ident->expr);

        if (stmt_ident->type.has_value()) { // it has an explicit declaration
            const auto var = m_vars.getVar(stmt_ident->ident.value.value()).value();
            var->type = stmt_ident->type.value();
        }

        m_vars.pop("eax");
        // the location of the var in the stack
        auto const pos = m_vars.getVarsSize() - m_vars.getVarLoc(stmt_ident->ident.value.value());
        m_output << "    mov DWORD [esp + " << pos * 4 << "], eax\n";
    }

    /**
 * @brief Generates code to print a single character that is already on the stack.
 *
 * This function generates assembly code to invoke the `WriteConsoleA` function from the Windows API,
 * which prints a character to the console. The character to be printed is assumed to be already on the stack.
 * After the character is printed, the stack is cleaned by popping the value off the stack.
 *
 */
    void print_char() {
        auto *gen = this;
        gen->m_output << "    invoke GetStdHandle, STD_OUTPUT_HANDLE\n";
        gen->m_output << "    lea ebx, [esp]\n";
        gen->m_output << "    invoke WriteConsoleA, eax, ebx, 4, 0 ,0\n";
        gen->m_vars.pop();
    }

    /**
 * @brief Generates assembly code to print an integer that is already on the stack.
 *
 * This function converts an integer into its character representation by repeatedly dividing
 * the integer by 10 and storing the remainder (which corresponds to a digit) on the stack.
 * This process continues until all digits are processed. If the integer is negative, a minus
 * sign is added at the beginning of the character string. The integer is then printed using
 * the `WriteConsoleA` function, and the stack is cleaned afterward.
 *
 * The process is as follows:
 * 1. Save the integer value on the stack for later.
 * 2. If the integer is negative, convert it to positive.
 * 3. Convert the integer to a string of characters representing the digits.
 * 4. If the integer is negative, push a minus sign to the stack.
 * 5. Print the resulting string using `WriteConsoleA`.
 * 6. Clean the stack by removing the characters and integer.
 */
    void print_int() {


        auto* gen = this;
        std::string const start = gen->label();
        std::string const end = gen->label();
        std::string const pos_int = gen->label();
        std::string const print = gen->label();

        gen->m_vars.pop("eax");
        gen->m_output << "    push eax\n"; // save the int value for later
        gen->m_output << "    xor ecx, ecx\n";
        gen->m_output << "    mov ebx, 10\n";

        // in case the int is negative convert to positive
        gen->m_output << "    cmp eax, 0\n";
        gen->m_output << "    jge " << pos_int <<"\n";
        gen->m_output << "    mov ecx, -1\n";
        gen->m_output << "    mul ecx\n";
        gen->m_output << "    mov ecx, 0\n";

        // convertion from int to chars
        gen->m_output << pos_int <<":\n";
        gen->m_output << start << ":\n";
        gen->m_output << "    xor edx, edx\n";
        gen->m_output << "    div ebx\n";
        gen->m_output << "    add edx, 48\n";
        gen->m_output << "    push edx\n";
        gen->m_output << "    add ecx, 4\n";
        gen->m_output << "    cmp eax, 0\n";
        gen->m_output << "    je " << end << "\n";
        gen->m_output << "    jmp " << start << "\n";

        // add '-' to the string in case the int is negative
        gen->m_output << end << ":\n";
        gen->m_output << "    cmp DWORD[esp + ecx], 0\n";
        gen->m_output << "    jge " << print <<"\n";
        gen->m_output << "    push '-'\n";
        gen->m_output << "    add ecx, 4\n";

        // print the string (int)
        gen->m_output << print <<":\n";
        gen->m_output << "    invoke GetStdHandle, STD_OUTPUT_HANDLE\n";
        gen->m_output << "    lea ebx, [esp]\n";
        gen->m_output << "    push ecx\n";
        gen->m_output << "    invoke WriteConsoleA, eax, ebx, ecx, 0 ,0\n";
        gen->m_output << "    pop ecx\n";

        gen->m_output << "    add ecx, 4\n"; // to delete the string stored in stack plus the initial int

        // restore the stack size
        gen->m_output << "    add esp, ecx\n";
    }

    /**
     * @brief Generates the assembly code to call a function with its arguments.
     *
     * This function handles the generation of assembly code for calling a function, including
     * the evaluation of its arguments. It first processes each argument expression, then starts
     * the function by setting up the necessary variables, and finally calls the function. After the
     * function call, the stack is cleaned by removing the arguments that were pushed.
     *
     * The process is as follows:
     * 1. Each argument expression is evaluated by calling `gen_expr` on the argument.
     * 2. The function is started by calling `startFun`, which sets up the necessary function context
     *    (e.g., checking if the function exists and verifying the number of arguments).
     * 3. The function is called using `call fun_<function_name>`, generating the appropriate assembly code.
     * 4. Finally, the stack is cleaned by popping the arguments that were pushed onto the stack.
     *
     * @tparam T The type of the function identifier, typically a structure that contains function
     *          name and arguments.
     *
     * @param fun_ident A pointer to the function identifier which includes the function name and
     *                  the list of arguments.
     */
    template <typename T>
    void gen_fun_ident(T* fun_ident) {

        for(size_t i = 0; i < fun_ident->args.size(); i++ ) {
            gen_expr(fun_ident->args[i]);
        }
        m_vars.startFun(fun_ident);

        m_output << "    call fun_" << fun_ident->name << "\n";

        m_vars.pop(fun_ident->args.size());
    }

    /**
 * @brief Generates the assembly code for a function declaration.
 *
 * This function handles the generation of assembly code for declaring a function, setting up
 * the function context, and managing the stack and function variables. It ensures that the function
 * is not executed accidentally unless explicitly called. The function also manages the function's
 * scope and performs stack cleanup after the function finishes.
 *
 * The process is as follows:
 * 1. Initializes `m_all_path_return` to false to ensure no previous results are used.
 * 2. Creates the function in `m_vars` (if it doesn't already exist) and sets up a new stack vector
 *    to track the function's local variables.
 * 3. A label `end` is inserted to prevent accidental execution of the function, unless it's explicitly
 *    called.
 * 4. The function body is generated by calling `gen_scope` to generate the statements within the function.
 * 5. The function concludes by generating a `ret` instruction and cleaning up the stack.
 * 6. After the function has finished, the local variables and function arguments are removed from the stack.
 * 7. Sets `m_currentFun` to `nullptr` to indicate no function is currently being processed.
 *
 * @param stmt_fun A pointer to the function statement (`NodeStmtFun`), which contains the function's
 *                 name, body (scope), and other related details.
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

    /**
 * @brief Generates the assembly code for a statement inside a scope.
 *
 * This function generates assembly code for various types of statements inside a scope.
 * It handles the generation of code for statements such as variable declarations, function calls,
 * control flow (if, for, while), printing expressions, and return statements.
 * It also ensures that certain statements (like function declarations) are not generated inside scopes
 * to avoid errors.
 *
 * @param stmt A pointer to the statement (`NodeStmt`) to be generated.
 *
 */
    void gen_stmt_norm(const NodeStmt*stmt) {
        struct StmtVisitor {
            Generator* gen;

            void operator()(const NodeStmtExit *stmt_exit) const {
                gen->gen_expr(stmt_exit->expr);
                gen->m_vars.pop("edi");
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
             * gens the expretion to print and according to the expretion's type
             * calls print int or print char
             */
            void operator()(const NodeStmtPrint* stmt_print) const {

                gen->gen_expr(stmt_print->expr);

                if (stmt_print->expr->type == typeChar) {
                    gen->print_char();
                }
                else {
                    gen->print_int();
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
                gen->m_vars.pop("eax");

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
            // gens the array's expretions (if exists) backwards
            // and tracks the new array in m_vars
            void operator()(const NodeStmtLetArray* stmt_let_array) const {

                // converting the expretions size to int
                const int sizeExpr = static_cast<int>(stmt_let_array->exprs.size());

                // if the arary's size is not specified simply gen the expretions
                if (!stmt_let_array->size.has_value()) {
                    for (int i = sizeExpr-1; i >= 0; i--) {
                        gen->gen_expr(stmt_let_array->exprs[i]);
                    }
                    gen->m_vars.newVar(stmt_let_array->ident.value.value(), stmt_let_array->type, sizeExpr);
                    return;
                }

                // converting the array's size from string to int
                const int sizeArray = stoi(stmt_let_array->size.value()->int_lit.value.value());

                // if the size is less than the expretions throw an error
                if (sizeArray < stmt_let_array->exprs.size()){
                    std::cerr << "Error array's expretions overflows array's size" <<std::endl;
                    exit(EXIT_FAILURE);
                }
                // else if the size is greater than the expretions
                if (sizeArray > stmt_let_array->exprs.size()) {
                    // make space for the uninitialize expretions and
                    // gen the ones that are initialized (if exist)
                    const int dif = sizeArray - static_cast<int>(stmt_let_array->exprs.size());
                    gen->m_vars.push(dif);
                    for (int i = sizeExpr-1; i >= 0; i--) {
                        gen->gen_expr(stmt_let_array->exprs[i]);
                    }
                    gen->m_vars.newVar(stmt_let_array->ident.value.value(), stmt_let_array->type, sizeArray);
                    return;
                }
                // else the array's size equals the number of expretions (just gen the expretions)
                for (int i = sizeExpr-1; i >= 0; i--) {
                    gen->gen_expr(stmt_let_array->exprs[i]);
                }
                gen->m_vars.newVar(stmt_let_array->ident.value.value(), stmt_let_array->type, sizeExpr);
            }

            void operator()(const NodeStmtArrayIndex* stmt_array_index) const {
                gen->gen_expr(stmt_array_index->expr);
                gen->m_vars.pop("ebx"); // value to change

                // compute the index in bytes
                gen->gen_expr(stmt_array_index->index);
                gen->m_vars.pop("eax");
                gen->m_output << "    mov ecx, 4\n";
                gen->m_output << "    mul ecx\n";

                // add to index the array's position to get the item's position
                size_t const pos = gen->m_vars.getVarPosBytes(stmt_array_index->ident.value.value());
                gen->m_output << "    add eax, " << pos << "\n";

                // overwrite the value
                gen->m_output << "    mov DWORD [esp + eax], ebx\n";
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
 * @brief Generates the assembly code for a statement outside a scope.
 *
 * This function generates assembly code for statements that are not inside a scope.
 * It handles statements like function declarations. It ensures that function declarations
 * and other similar statements are not generated inside scopes, where such operations are not allowed.
 *
 * @param stmt A pointer to the statement (`NodeStmt`) to be generated.
 *
 * This function delegates the generation of code for each statement type:
 * - For `NodeStmtFun`, it calls `gen_fun` to handle function declarations.
 * - For other types of statements, it calls `gen_stmt_norm` to handle them inside a scope.
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

    /**
 * @brief Generates the complete assembly code for the program.
 *
 * This function starts the generation of the assembly code for the entire program by iterating
 * through each statement in the previously generated parse tree. It sets up the necessary format,
 * imports, and sections to create a Windows PE (Portable Executable) console application.
 * Then, it generates the assembly code for each statement in the program using the `gen_stmt` function.
 * If no explicit exit or return is found in the program, it will automatically invoke `ExitProcess` with a code of 0.
 *
 * @return A string containing the complete assembly code.
 *
 * The function performs the following steps:
 * - Sets up the PE format and imports required for Windows API calls.
 * - Iterates over the list of program statements and generates assembly code for each.
 * - Ensures that the program exits cleanly by invoking `ExitProcess` if no return or exit is explicitly specified.
 */
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
 * @brief Generates a new label for jump instructions.
 *
 * This function generates a unique label name each time it is called, which is typically used for jump
 * instructions or other code flow control constructs that require a label.
 * Each generated label is named incrementally (e.g., "label1", "label2", etc.).
 *
 * @return A string representing the unique label name.
 *
 * @example
 *  string l1 = label();
 *  cout << l1; // Output: label1
 *  string l2 = label();
 *  cout << l2; // Output: label2
 *
 * The function is useful for generating unique jump targets within assembly code to manage control flow.
 */
    std::string label() {
        n_label++;
        return "label" + std::to_string(n_label);
    }
    int n_label{}; // keeps record of the number of labels

    /**
 * @brief A string stream used to generate the output assembly code.
 *
 * This member variable stores the assembly code as it is generated. The string stream is used to accumulate
 * the assembly instructions in a readable format which can later be written to a file or output for further use.
 *
 * @note This is used extensively during code generation to build the complete assembly program.
 */
    std::stringstream m_output;

    /**
 * @brief The parse tree representing the source program.
 *
 * This member variable holds the abstract syntax tree (AST) of the source code that was parsed. It represents
 * the structure and hierarchy of the program, and the code generation process iterates through this tree to
 * produce the corresponding assembly code.
 *
 */
    const NodeProg m_prog;


    /**
 * @brief Keeps track of variables, functions, and their elements (arguments, sizes, locations, etc.).
 *
 * This member variable is responsible for managing the scope and tracking the information related to variables,
 * function arguments, and functions themselves during the code generation process. It handles the tracking of
 * variable locations, sizes, and the organization of functions and their respective elements.
 *
 * @note `m_vars` ensures proper handling of variables within different scopes, managing their lifetimes and
 *       ensuring that variables and functions are correctly generated and placed in the assembly code.
 */
    Vars m_vars;

    /**
 * @brief Keeps track of the number of nested scopes at the moment.
 *
 * This variable keeps a count of the number of nested scopes that are currently active during the code
 * generation process. It helps determine the scope depth, ensuring that variables and statements are generated
 * correctly for each scope level.
 */
    size_t m_number_scopes{};

    // TODO delete this shit and figure something better
    /**
 * @brief Tracks whether the current function being generated always returns a value.
 *
 * This boolean variable is used to check if every execution path in the current function leads to a return
 * value. It is currently implemented with the intention of simplifying control flow analysis in the code
 * generation. However, this approach might be suboptimal and needs refinement in future updates (as indicated
 * by the TODO comment).
 *
 * @note The current implementation is temporary and will be replaced with a better solution in the future.
 */
    bool m_all_path_return{};


};
