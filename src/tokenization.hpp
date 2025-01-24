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
    less,
    char_lit,
    _for,
    _while,
    print,
    fun,
    comma,
    _return,
    colon,
    typeInt,
    typeChar,
    typeVoid
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

    /**
    *
    * Separates the source code string in Tokens by itering char to char
*/
    inline std::vector<Token> tokenize()
    {
        std::vector<Token> tokens;

        while (currentChar().has_value()) {
            // To tokenize reserved words or idents
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
                else if (buf == "for") {
                    tokens.push_back(Token{TokenType::_for});
                }
                else if (buf == "while") {
                    tokens.push_back(Token{TokenType::_while});
                }
                else if (buf == "print") {
                    tokens.push_back(Token{TokenType::print});
                }
                else if (buf == "fun") {
                    tokens.push_back(Token{TokenType::fun});
                }
                else if (buf == "return") {
                    tokens.push_back(Token{TokenType::_return});
                }
                else if (buf == "int") {
                    tokens.push_back(Token{TokenType::typeInt});
                }
                else if (buf == "char") {
                    tokens.push_back(Token{TokenType::typeChar});
                }
                else if (buf == "void") {
                    tokens.push_back(Token{TokenType::typeVoid});
                }
                else {
                    tokens.push_back(Token{TokenType::ident, buf});
                }
                buf.clear();

            }
            // to tokenize numbers
            else if (std::isdigit(currentChar().value())) {
                consume2buf();
                while (currentChar().has_value() && std::isdigit(currentChar().value())) {
                    consume2buf();
                }
                tokens.push_back(Token{TokenType::int_lit, buf});
                buf.clear();
            }
            // to tokenize chars, the char \' is '
            else if (currentChar().value() == '\'') {  // '
                m_index++;
                if (!currentChar().has_value()) {
                    std::cerr << "Error: expected char after ' " << std::endl;
                    exit(EXIT_FAILURE);
                }
                std::string  char_lit;
                // append to char_lit the char
                char_lit.push_back(currentChar().value());
                m_index++;
                if (currentChar().has_value() && currentChar().value() == '\'') {
                    tokens.push_back(Token{TokenType::char_lit, char_lit});
                    m_index++;
                }
                else {
                    std::cerr << "Error: expected ' after char" << std::endl;
                    exit(EXIT_FAILURE);
                }
            }
            // to tokenize special characters / grammar punctuations
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

                    case ',':
                        tokens.push_back(Token{TokenType::comma});
                        m_index++;
                        break;

                    case ':':
                        tokens.push_back(Token{TokenType::colon});
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

        /**
         * Ignore, this is the previous vertion for the tokenize function,
         * still here because idk which is best optimized
         */

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
    /**
    * if the index is in the range of the source code string's (m_src) size then
    * return the char at that index else return non value
*/
    [[nodiscard]] inline std::optional<char> currentChar() const
    {
        if (m_index  < m_src.size()) {
            return m_src[m_index];
        }
        return {};
    }

    /**
    * append to the buf string the current char in the source code string (m_src)
*/
    inline void consume2buf() {
        buf.push_back(m_src[m_index++]);
    }

    // the buffer which will be used to temporally contain the reserved words found in the source code
    std::string buf;
    // the source code
    const std::string m_src;
    // the index used to iter threw the source code (m_src)
    size_t m_index{};
};


