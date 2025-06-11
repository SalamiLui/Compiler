#pragma once
#include <iostream>
#include <ostream>

#include "parser.hpp"
#include "tokenization.hpp"
#include <string>

class Vars {

private:

    /**
 * @struct Var
 * @brief Structure representing a variable in the context of the stack.
 *
 * This structure holds the data associated with a variable, such as its name,
 * stack location, type, and size.
 */
    struct Var { // NOLINT(*-pro-type-member-init)
        std::string name;
        size_t stack_loc;
        mutable TokenType type;
        size_t var_size{1};
    };

    /**
 * @brief Represents the set of variables in a function.
 *
 * This structure holds the list of variables declared within a function and the
 * total stack size used for storing those variables.
 */
    struct Stack {
        std::vector<Var> vars;
        size_t stack_size = 0;
    };

    /**
  * @brief Represents a function.
  *
  * This structure stores information about a function, including its name,
  * the list of arguments, and the return type.
  */
    struct Fun {
        std::string name;
        std::vector<Token> args;
        TokenType returnType;
    };

    /**
    // return true if the stack size >= the vector of vars
    // (if the stack size if less tan the vector of vars then you should use first delVars)
    // to avoid popping from stack vars by accident
    [[nodiscard]]bool isPermitedPop (size_t const n = 1)const {
        if (m_varsStack.back().stack_size - n >= m_varsStack.back().vars.size()) {
            return true;
        }
        return false;
    }
    */

public:
    std::stringstream &m_output;

    /**
 * @brief Vector that tracks variables, stack size, and related information.
 *
 * This vector holds the stack frames (`Stack`) that track the variables within
 * a function's scope. A new stack is pushed when a new function is created, and
 * the last stack is popped when the function scope ends. The last item in the
 * vector always represents the currently active function scope, and it is used
 * to manage the function's variables and stack details.
 */
    std::vector<Stack> m_varsStack;

    /**
    * @brief Vector that tracks functions and their elements.
    *
    * This vector holds the details of all the functions, including their names,
    * parameters, return types, etc. Each `Fun` object represents a single function
    * in the program.
    */
    std::vector<Fun> m_funs;

    /**
 * @brief Vector that tracks the size of function arguments TODO delete this, is trash.
 *
 * This vector is used to track the size of the arguments for each function.
 * However it is considered redundant and should not be used.
 */
    std::vector<size_t> m_funs_args_size;

    /**
 * @brief Pointer to the current function being generated.
 *
 * This pointer holds a reference to the currently active function being
 * processed or generated. If no function is currently being processed,
 * it remains as a `nullptr`.
 */
    Fun const *m_currentFun = nullptr;

    explicit Vars(std::stringstream &output): m_output(output) {
        m_varsStack.emplace_back();
        m_funs_args_size.emplace_back(0);
    }


    /**
 * @brief Searches for a function by its name.
 *
 * This method searches through the list of functions (`m_funs`) to find a
 * function with the specified name. If a function is found, it returns a
 * pointer to the function. If no such function exists, it returns an empty
 * `std::optional`.
 *
 * @param name The name of the function to search for.
 * @return A `std::optional<const Fun*>` containing the function pointer if
 *         the function is found, or an empty `std::optional` if no match is found.
 */
    [[nodiscard]]std::optional<const Fun*> getFun (std::string const &name) const{
        for (auto const &fun : m_funs) {
            if (fun.name == name) {
                Fun const *pFun = &fun;
                return pFun;
            }
        }
        return {};
    }

    /**
 * @brief Searches for a variable by its name in the current stack.
 *
 * This method searches for a variable in the most recent stack (`m_varsStack.back()`).
 * If the variable is found, it returns a pointer to the variable. If the variable
 * is not found in the last stack, it returns an empty `std::optional`.
 *
 * @param name The name of the variable to search for.
 * @return A `std::optional<const Var*>` containing the variable pointer if
 *         the variable is found in the last stack, or an empty `std::optional`
 *         if the variable is not found.
 */
    [[nodiscard]]std::optional<const Var*> getVar(std::string const &name) const{

        for (auto const &var : m_varsStack.back().vars) {
            if (var.name == name) {
                Var const *pVar = &var;
                return pVar;
            }
        }
        return {};
    }

    /**
 * @brief Retrieves the stack location of a variable.
 *
 * This method attempts to find a variable by its name in the current stack.
 * If the variable is found, it returns the variable's stack location. If the
 * variable is not found, an error message is printed and the program exits.
 *
 * @param name The name of the variable whose stack location is to be retrieved.
 * @return The stack location of the variable.
 * @throws std::runtime_error if the variable is not found.
 */
    [[nodiscard]]size_t getVarLoc(std::string const &name) const {
        auto const var = getVar(name);
        if (!var.has_value()) {
            std::cerr << "Error variable " << name  << " not found" << std::endl;
        }
        return var.value()->stack_loc;
    }

    /**
 * @brief Calculates the byte offset of a variable relative to the ESP.
 *
 * This method calculates the distance from the current position of the stack
 * (represented by ESP) to the stack location of the specified variable.
 * The stack's size is multiplied by 4 (assuming each stack entry is 4 bytes)
 * to determine the byte offset.
 *
 * @param name The name of the variable whose byte offset from ESP is to be calculated.
 * @return The byte offset (distance) from the ESP to the variable's stack location.
 */
    [[nodiscard]]size_t getVarPosBytes(std::string const &name) const {
        return (getVarsSize() - getVarLoc(name)) * 4;
    }

    /**
 * @brief Increments the size of the last stack and pushes a register onto the stack.
 *
 * This method increments the stack size by one and generates the corresponding
 * assembly instruction to push the specified register onto the stack.
 * The instruction is written to the output stream in assembly format.
 *
 * @param reg The name of the register to be pushed onto the stack (e.g., "eax", "ebx").
 */
    void push(std::string const &reg) {
        m_varsStack.back().stack_size++;
        m_output << "    push " << reg << "\n";
    }

    /**
 * @brief Increments the size of the last stack by a specified number of times and generates an assembly instruction to adjust the stack pointer.
 *
 * This method increments the stack size by the specified number of `times` and writes the corresponding
 * assembly instruction to subtract `times * 4` from the ESP register. This is commonly used when declaring
 * uninitialized variables on the stack, reserving space for them without initializing the values.
 *
 * @param times The number of 4-byte units to reserve on the stack (each unit represents one variable).
 */
    void push(size_t const times) {
        m_varsStack.back().stack_size += times;
        m_output << "    sub esp, " << times * 4 << "\n";
    }

    /**
 * @brief Decrements the size of the last stack by one and generates an assembly instruction to pop the value into the specified register.
 *
 * This method decreases the stack size by one and generates the corresponding assembly instruction to pop
 * a value from the stack into the specified register. It is used to remove the most recently pushed value
 * from the stack and store it in the provided register.
 *
 * @param reg The register into which the popped value will be placed (e.g., `eax`, `ebx`, etc.).
 */
    void pop(std::string const &reg) {

        m_varsStack.back().stack_size--;
        m_output <<  "    pop " << reg << "\n";
    }

    /**
 * @brief Decrements the size of the last stack by one and generates an assembly instruction to add 4 bytes to the stack pointer (esp).
 *
 * This method decreases the stack size by one and generates the corresponding assembly instruction to adjust
 * the stack pointer (esp) by 4 bytes, effectively removing the top element from the stack.
 *
 * @see pop(std::string const &reg) For the version of the `pop` method that pops a value into a specified register.
 */
    void pop() {
        m_varsStack.back().stack_size--;
        m_output<< "    add esp, 4\n";
    }

    /**
  * @brief Decrements the size of the last stack by a specified number and generates an assembly instruction to add the stack pointer (esp) by `times * 4` bytes.
  *
  * This method decreases the stack size by the specified number of variables (`times`) and generates the corresponding
  * assembly instruction to adjust the stack pointer (esp) by `times * 4` bytes, effectively removing the top `times`
  * elements from the stack.
  *
  * @param times The number of variables to remove from the stack. Each variable corresponds to 4 bytes of space.
  *
  * @see pop() For the version of the `pop` method that removes a single variable from the stack.
  * @see pop(std::string const &reg) For the version of the `pop` method that pops a value into a specified register.
  */
    void pop(size_t const times) {
        m_varsStack.back().stack_size -= times;
        std::string const bytes = std::to_string((times * 4));
        m_output << "    add esp, " << bytes << "\n";
    }


    /**
 * @brief Retrieves the current size of the last stack in the `m_varsStack`.
 *
 * This method returns the current size of the stack at the top of the `m_varsStack` vector. The size reflects the
 * number of variables tracked within the current scope.
 *
 * @return The size of the last stack in `m_varsStack`, indicating the number of variables currently being tracked in
 *         the scope.
 */
    [[nodiscard]]size_t getVarsSize() const {
        return m_varsStack.back().stack_size;
    }

    /**
 * @brief Checks if a variable exists in the last stack in `m_varsStack`.
 *
 * This method checks if a variable with the given name is present in the current scope, which is represented by the
 * last stack in the `m_varsStack`. It looks for the variable in the most recent scope and returns `true` if found.
 *
 * @param name The name of the variable to check for.
 * @return `true` if the variable is found in the last stack, `false` otherwise.
 */
    [[nodiscard]]bool isInVars(std::string const &name) const {
        return getVar(name).has_value();
    }


    /**
 * @brief Deletes a specified number of variables from the last stack.
 *
 * This method removes the specified number of variables from the most recent stack in `m_varsStack`. It ensures that
 * the number of variables to be deleted does not exceed the current size of the stack. If the number of variables
 * to be deleted is greater than the stack's current size, an error message is printed, and the program exits.
 *
 * @param number The number of variables to delete from the stack.
 * @throws std::exit If the number of variables to delete exceeds the current stack size, the program exits.
 */
    void deleteVars(size_t const number) {

        if (number > m_varsStack.back().stack_size) {
            std::cerr << "Vars stack overflow" << std::endl;
            exit(EXIT_FAILURE);
        }
        size_t i{};
        while (i < number) {
            const size_t vars2del = m_varsStack.back().vars.back().var_size;
            m_output << "    add esp, " << vars2del * 4<< "\n";
            i += vars2del;
            m_varsStack.back().vars.pop_back();
        }
        m_varsStack.back().stack_size -= number;

    }

    /**
 * @brief Appends a new variable to the last stack, if it doesn't already exist.
 *
 * This method adds a new variable to the most recent stack in `m_varsStack`. If the variable already exists in the
 * stack, an error message is printed, and the program exits.
 *
 * @Note Before calling `newVar`, ensure that the expression is pushed to the stack using the `push` function,
 *       or manually increment the `stack_size` if needed.
 *
 * @param name The name of the variable to be added.
 * @param type The type of the variable (default is `typeInt`).
 * @param size The size of the variable (default is `1`).
 * @throws std::exit If the variable already exists in the stack, the program exits.
 */
    void newVar(std::string const &name, TokenType const &type = typeInt, size_t const size = 1) {

        size_t const stack_size = m_varsStack.back().stack_size;

        if (getVar(name)) {
            std::cerr << "Variable "<< name <<" already exists" << std::endl;
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().vars.push_back(
            Var{
                .name = name,
                .stack_loc = stack_size,
                .type = type,
                .var_size = size
            }
        );
    }


    /**
 * @brief Checks if the function exists and if the number of arguments is correct.
 *
 * This method validates whether a function exists in the current context and whether it is called with the correct
 * number of arguments. If the function is not found or the number of arguments does not match, an error message is
 * printed, and the program exits.
 *
 * @Note Ensure that the necessary expressions are pushed to the stack before calling this method.
 *
 * @tparam T The type of the function object being passed.
 * @param fun Pointer to the function object being validated.
 * @throws std::exit If the function is not found or the argument count is incorrect.
 */
    template <typename T>
    void startFun(T* fun) {


        auto optionalFun = getFun(fun->name);

        if (!optionalFun.has_value()) {
            std::cerr << "Function "<< fun->name <<" not found" << std::endl;
            exit(EXIT_FAILURE);
        }

        Fun const *pFun = optionalFun.value();


        if (fun->args.size() != pFun->args.size()) {
            std::cerr << "Function "<< fun->name << " called with wrong numner of arguments\n";
            exit(EXIT_FAILURE);
        }

    }


    /**
 * @brief Registers a new function and prepares the variable stack for scope generation.
 *
 * This function registers a new function in the list of functions (`m_funs`) and prepares the variable stack
 * (`m_varsStack`) for the scope of the function. It ensures that no expression or stack manipulation is needed
 * before calling this method. The function creates a new scope, adds function arguments to the stack, and tracks
 * the return variable (`ret`).
 * @Note Once the function's scope is generated, make sure to pop the stack and reset
 *       relevant variables, including `m_currentFun`, after the function is processed.
 *
 * @param fun Pointer to the function's AST node (`NodeStmtFun`) that contains function details.
 * @throws std::exit If the function already exists in the function list (`m_funs`).
 */
    void newFun(NodeStmtFun const *fun) {
        if (getFun(fun->name).has_value()) {
            std::cerr << "Error: Function " << fun->name << " already exists" << std::endl;
            exit(EXIT_FAILURE);
        }
        m_funs.push_back(
            Fun{
                .name = fun->name,
                .args = fun->idents,
                .returnType = fun->returnType
            }
        );

        m_varsStack.emplace_back();
        m_funs_args_size.push_back(fun->idents.size());

        int indx{};
        for (const Token& arg : fun->idents) {
            m_varsStack.back().stack_size++;

            newVar(arg.value.value(), fun->types[indx].has_value() ? fun->types[indx].value() : typeInt);
            indx++;
        }

        // when used 'call' asmb pushes the ret direction to stack, so it's necessary track it
        // to avoid corrupt the stack
        m_varsStack.back().stack_size++;
        newVar("@ret");

        m_currentFun = &m_funs.back();
    }
};
