#pragma once
#include <iostream>
#include <ostream>

#include "parser.hpp"
#include "tokenization.hpp"
#include <string>

class Vars {

private:

    // a variable
    struct Var { // NOLINT(*-pro-type-member-init)
        std::string name;
        size_t stack_loc;
        mutable TokenType type;
    };

    // the set of vars in a function
    struct Stack {
        std::vector<Var> vars;
        size_t stack_size = 0;
    };

    // a function
    struct Fun {
        std::string name;
        std::vector<Token> args;
        TokenType returnType;
    };

    // return true if the stack size >= the vector of vars
    // (if the stack size if less tan the vector of vars then you should use first delVars)
    // to avoid popping from stack vars by accident
    [[nodiscard]]bool isPermitedPop (size_t const n = 1)const {
        if (m_varsStack.back().stack_size - n >= m_varsStack.back().vars.size()) {
            return true;
        }
        return false;
    }

public:
    /**
    * vector where vars, stack_size, etc is tracked
    * a new stack is pushed back to track a new set of vars
    * usually when a new function is created, once the function ended
    * the last item is deleted,
    * The last item is always the one that is being used to keep tracked the vars
    */
    std::vector<Stack> m_varsStack;


    // vector where functions and its elements are keep tracked
    std::vector<Fun> m_funs;
    // vector where functions args's size is tracked TODO delete this, is trash
    std::vector<size_t> m_funs_args_size;
    // the current function that is being generated
    Fun const *m_currentFun = nullptr;

    // TODO add a pointer to the output stringstream for easier use of push and pop functions
    Vars() {
        m_varsStack.emplace_back();
        m_funs_args_size.emplace_back(0);
    }

    // returns a pointer to the function if it's found otherwise returns empty
    [[nodiscard]]std::optional<const Fun*> getFun (std::string const &name) const{
        for (auto const &fun : m_funs) {
            if (fun.name == name) {
                Fun const *pFun = &fun;
                return pFun;
            }
        }
        return {};
    }

    // returns a pointer to the var if it's found in the last stack, otherwise returns empty
    [[nodiscard]]std::optional<const Var*> getVar(std::string const &name) const{

        for (auto const &var : m_varsStack.back().vars) {
            if (var.name == name) {
                Var const *pVar = &var;
                return pVar;
            }
        }
        return {};
    }

    // returns the var's location, throws an error if the var doesn't exist
    [[nodiscard]]size_t getVarLoc(std::string const &name) const {
        auto const var = getVar(name);
        if (!var.has_value()) {
            std::cerr << "Error variable " << name  << " not found" << std::endl;
        }
        return var.value()->stack_loc;
    }

    // returns the var's distance to the esp's position in bytes
    [[nodiscard]]size_t getVarPosBytes(std::string const &name) const {
        return (getVarsSize() - getVarLoc(name)) * 4;
    }

    // increments the last stack size by one, returns the asmb instruction to push the selected register
    std::string [[nodiscard]]push(std::string const &reg) {
        m_varsStack.back().stack_size++;
        return "    push " + reg + "\n";
    }

    // decreases the last stack size by one, returns the asm instruction to pop to the selected register
    // if trying to delete a tracked var in stack without using first delVar will throw an exception
    std::string [[nodiscard]]pop(std::string const &reg) {
        if (!isPermitedPop()) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size--;
        return "    pop " + reg + "\n";
    }

    // decreases the last stack size by one, returns the asm instruction to add the esp 4 bytes (1 var)
    // if trying to delete a tracked var in stack without using first delVar will throw an exception
    std::string [[nodiscard]]pop() {
        if (!isPermitedPop()) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size--;
        return "    add esp, 4\n";
    }

    // decreases the last stack size by one, returns the asm instruction to add the esp times*4 bytes (times vars)
    // if trying to delete a tracked var in stack without using first delVar will throw an exception
    std::string [[nodiscard]]pop(size_t const times) {
        if (!isPermitedPop(times)) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size -= times;
        std::string const bytes = std::to_string((times * 4));
        return "    add esp, " + bytes + "\n";
    }


    // returns the current size of the last stack in m_varsStack
    [[nodiscard]]size_t getVarsSize() const {
        return m_varsStack.back().stack_size;
    }

    // returns true if the var is in the last stack
    [[nodiscard]]bool isInVars(std::string const &name) const {
        return getVar(name).has_value();
    }


    /**
     * pops the specified number of vars from the last stack
     * throws an exception if the number is greater than the current stack's size
     * be shure to use pop AFTER this in order to actually delete the vars from memory
    */
    void deleteVars(size_t const number) {
        if (number > m_varsStack.back().stack_size) {
            std::cerr << "Vars stack overflow" << std::endl;
            exit(EXIT_FAILURE);
        }
        for (int i = 0; i < number; ++i) {
            m_varsStack.back().vars.pop_back();
        }
    }

    /*
     *  appends a new var (if it doesn't already exist) to the last stack
     *  Before call newVar make shure to push the expretion using the function push
     *  or if necessary increment manually the stack_size before calling it
     */

    void newVar(std::string const &name, TokenType const &type = typeInt) {

        size_t const stack_size = m_varsStack.back().stack_size;

        if (stack_size - 1 != m_varsStack.back().vars.size()) {
            std::cerr << "Vars stack corrupted" << std::endl;
            exit(EXIT_FAILURE);
        }

        if (getVar(name)) {
            std::cerr << "Variable "<< name <<" already exists" << std::endl;
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().vars.push_back(
            Var{
                .name = name,
                .stack_loc = stack_size,
                .type = type
            }
        );
    }


    /*
     *  checks if the function exists and if the number of arguments is correct
     *  if not an exception is thrown
        Be shure to push the expretions needed for the function
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


    /*
     *  The function register the fun in m_funs and prepares m_varsStack for gen the scope
     *  It's not necessary to gen any expression or push something to stack before calling this
     *  Once generated the scope be shure to pop back m_varsStack, m_funs_args_size and set
     *  m_currentFun to nullptr
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
