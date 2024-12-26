#pragma once

#include <string>
#include <vector>
#include <optional>

enum TokenType {
    __exit,  // NOLINT(*-reserved-identifier)
    int_lit,
    semi,
    open_paren,
    close_paren,
    let,
    ident,
    eq,
    plus,
    star,
    sub,
    slash,
    open_curly,
    close_curly,
    _if,
    _else,
    exc_mark,
    great,
    less
};

inline int bin_prec(TokenType const type) {
    switch (type) {
        case TokenType::plus:
            return 1;
        case TokenType::sub:
            return 2;
        case TokenType::star:
            return 3;
        case TokenType::slash:
            return 4;
        default:
            return -1;
    }
};

struct Token {
    TokenType type;
    std::optional<std::string> value;
};

class Tokenizer
{
public:
    inline explicit Tokenizer(std::string& src)
        : m_src(std::move(src))
    {
    }

    inline std::vector<Token> tokenize()
    {
        std::vector<Token> tokens;

        while (currentChar().has_value()) {
            if (std::isalpha(currentChar().value())) {
                consume2buf();
                while (currentChar().has_value() && std::isalnum(currentChar().value())) {
                    consume2buf();
                }
                if (buf == "exit") {
                    tokens.push_back(Token{TokenType::__exit});
                }
                else if (buf == "let") {
                    tokens.push_back(Token{TokenType::let});
                }
                else if (buf == "if") {
                    tokens.push_back(Token{TokenType::_if});
                }
                else if (buf == "else") {
                    tokens.push_back(Token{TokenType::_else});
                }
                else {
                    tokens.push_back(Token{TokenType::ident, buf});
                }
                buf.clear();

            }
            else if (std::isdigit(currentChar().value())) {
                consume2buf();
                while (currentChar().has_value() && std::isdigit(currentChar().value())) {
                    consume2buf();
                }
                tokens.push_back(Token{TokenType::int_lit, buf});
                buf.clear();
            }
            else {
                switch (currentChar().value()) {
                    case ';':
                        tokens.push_back(Token{TokenType::semi});
                        m_index++;
                        break;

                    case '(':
                        tokens.push_back(Token{TokenType::open_paren});
                        m_index++;
                        break;

                    case ')':
                        tokens.push_back(Token{TokenType::close_paren});
                        m_index++;
                        break;

                    case '=':
                        tokens.push_back(Token{TokenType::eq});
                        m_index++;
                        break;

                    case '+':
                        tokens.push_back(Token{TokenType::plus});
                        m_index++;
                        break;

                    case '*':
                        tokens.push_back(Token{TokenType::star});
                        m_index++;
                        break;

                    case '-':
                        tokens.push_back(Token{TokenType::sub});
                        m_index++;
                        break;

                    case '/':
                        tokens.push_back(Token{TokenType::slash});
                        m_index++;
                        break;

                    case '!':
                        tokens.push_back(Token{TokenType::exc_mark});
                        m_index++;
                        break;

                    case '>':
                        tokens.push_back(Token{TokenType::great});
                        m_index++;
                        break;

                    case '<':
                        tokens.push_back(Token{TokenType::less});
                        m_index++;
                        break;

                    case '{':
                        tokens.push_back(Token{TokenType::open_curly});
                        m_index++;
                        break;

                    case '}':
                        tokens.push_back(Token{TokenType::close_curly});
                        m_index++;
                        break;

                    case ' ':
                        m_index++;
                        break;

                    case '\n':
                        m_index++;
                        break;

                    default:
                        std::cerr << "You entered an invalid token: "  << std::endl;
                        exit(EXIT_FAILURE);
                }
            }
        }
        m_index = 0;
        return tokens;

        /*
        while (currentChar().has_value()) {
            if (std::isalpha(currentChar().value())) {
                consume2buf();
                while (currentChar().has_value() && std::isalnum(currentChar().value())) {
                    consume2buf();
                }
                if (buf == "exit") {
                    tokens.push_back(Token{TokenType::__exit});
                }
                else if (buf == "let") {
                    tokens.push_back(Token{TokenType::let});
                }
                else if (buf == "if") {
                    tokens.push_back(Token{TokenType::_if});
                }
                else if (buf == "else") {
                    tokens.push_back(Token{TokenType::_else});
                }
                else {
                    tokens.push_back(Token{TokenType::ident, buf});
                }
                buf.clear();

            }
            else if (std::isdigit(currentChar().value())) {
                consume2buf();
                while (currentChar().has_value() && std::isdigit(currentChar().value())) {
                    consume2buf();
                }
                tokens.push_back(Token{TokenType::int_lit, buf});
                buf.clear();
            }
            else if (currentChar().value() == ';') {
                tokens.push_back(Token{TokenType::semi});
                m_index++;
            }
            else if (currentChar().value() == '(') {
                tokens.push_back(Token{TokenType::open_paren});
                m_index++;
            }
            else if (currentChar().value() == ')') {
                tokens.push_back(Token{TokenType::close_paren});
                m_index++;
            }
            else if (currentChar().value() == '=') {
                tokens.push_back(Token{TokenType::eq});
                m_index++;
            }
            else if (currentChar().value() == '+') {
                tokens.push_back(Token{TokenType::plus});
                m_index++;
            }
            else if (currentChar().value() == '*') {
                tokens.push_back(Token{TokenType::star});
                m_index++;
            }
            else if (currentChar().value() == '-') {
                tokens.push_back(Token{TokenType::sub});
                m_index++;
            }
            else if (currentChar().value() == '/') {
                tokens.push_back(Token{TokenType::slash});
                m_index++;
            }
            else if (currentChar().value() == '!') {
                tokens.push_back(Token{TokenType::exc_mark});
                m_index++;
            }
            else if (currentChar().value() == '>') {
                tokens.push_back(Token{TokenType::great});
                m_index++;
            }
            else if (currentChar().value() == '<') {
                tokens.push_back(Token{TokenType::less});
                m_index++;
            }
            else if (currentChar().value() == '{') {
                tokens.push_back(Token{TokenType::open_curly});
                m_index++;
            }
            else if (currentChar().value() == '}') {
                tokens.push_back(Token{TokenType::close_curly});
                m_index++;
            }
            else if (std::isspace(currentChar().value())) {
                m_index++;
            }
            else {
                std::cerr << "You entered an invalid token: "  << std::endl;
            }
        }

        */


    };

private:
    [[nodiscard]] inline std::optional<char> currentChar() const
    {
        if (m_index  < m_src.size()) {
            return m_src[m_index];
        }
        return {};
    }

    inline void consume2buf() {
        buf.push_back(m_src[m_index++]);
    }

    std::string buf;
    const std::string m_src;
    size_t m_index{};
};


