#pragma once
#include <iostream>
#include <ostream>

#include "parser.hpp"
#include "tokenization.hpp"
#include <string>

class Vars {

private:


    struct Var { // NOLINT(*-pro-type-member-init)
        std::string name;
        size_t stack_loc;
        mutable TokenType type;
    };

    struct Stack {
        std::vector<Var> vars;
        size_t stack_size = 0;
    };

    struct Fun {
        std::string name;
        std::vector<Token> args;
        TokenType returnType;
    };

    [[nodiscard]]bool isPermitedPop (size_t const n = 1)const {
        if (m_varsStack.back().stack_size - n >= m_varsStack.back().vars.size()) {
            return true;
        }
        return false;
    }

public:
    std::vector<Stack> m_varsStack;
    std::vector<Fun> m_funs;
    std::vector<size_t> m_funs_args_size;
    Fun const *m_currentFun = nullptr;

    Vars() {
        m_varsStack.emplace_back();
        m_funs_args_size.emplace_back(0);
    }

    [[nodiscard]]std::optional<const Fun*> getFun (std::string const &name) const{
        for (auto const &fun : m_funs) {
            if (fun.name == name) {
                Fun const *pFun = &fun;
                return pFun;
            }
        }
        return {};
    }

    [[nodiscard]]std::optional<const Var*> getVar(std::string const &name) const {

        for (auto const &var : m_varsStack.back().vars) {
            if (var.name == name) {
                Var const *pVar = &var;
                return pVar;
            }
        }
        return {};
    }

    [[nodiscard]]size_t getVarLoc(std::string const &name) const {
        auto const var = getVar(name);
        if (!var.has_value()) {
            std::cerr << "Error variable " << name  << " not found" << std::endl;
        }
        return var.value()->stack_loc;
    }

    [[nodiscard]]size_t getVarPosBytes(std::string const &name) const {
        return (getVarsSize() - getVarLoc(name)) * 4;
    }

    std::string [[nodiscard]]push(std::string const &reg) {
        m_varsStack.back().stack_size++;
        return "    push " + reg + "\n";
    }

    std::string [[nodiscard]]pop(std::string const &reg) {
        if (!isPermitedPop()) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size--;
        return "    pop " + reg + "\n";
    }

    std::string [[nodiscard]]pop() {
        if (!isPermitedPop()) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size--;
        return "    add esp, 4\n";
    }

    std::string [[nodiscard]]pop(size_t const times) {
        if (!isPermitedPop(times)) {
            std::cerr << "Unable to delete vars with pop\n";
            exit(EXIT_FAILURE);
        }
        m_varsStack.back().stack_size -= times;
        std::string const bytes = std::to_string((times * 4));
        return "    add esp, " + bytes + "\n";
    }


    [[nodiscard]]size_t getVarsSize() const {
        return m_varsStack.back().stack_size;
    }

    [[nodiscard]]bool isInVars(std::string const &name) const {
        return getVar(name).has_value();
    }


    // be shure to use pop AFTER this
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
     *  Once generated the scope be shure to pop back m_varsStack
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

        // when used 'call' asmb pushes the ret direction to stack, so it's necessary to track it
        // to avoid corrupt the stack
        m_varsStack.back().stack_size++;
        newVar("@ret");

        m_currentFun = &m_funs.back();
    }
};
