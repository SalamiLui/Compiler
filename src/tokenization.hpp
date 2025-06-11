#pragma once

#include <string>
#include <vector>
#include <optional>

/**
 * @brief Enum representing the different types of tokens.
 *
 * This enum is used to categorize the tokens found in the source code during
 * the tokenization process. Each value corresponds to a specific type of token,
 * such as operators, keywords, or identifiers.
 */
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
    open_bracket,
    close_bracket,
    typeInt,
    typeChar,
    typeVoid
};

/**
 * @brief Returns an integer representing the arithmetic precedence of a token.
 *
 * @param type The type of token to evaluate.
 * @return int The precedence value; returns -1 if the token is not an arithmetic operator.
 */
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
/**
 * @brief Represents a token in the source code.
 *
 * This structure is used to store information about individual tokens
 * found during the tokenization process, including its type and optional value.
 */
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
 * @brief Tokenizes the source code string by iterating character by character.
 *
 * This function scans the input source code and converts it into a vector
 * of tokens, recognizing identifiers, keywords, numbers, characters,
 * and punctuation symbols.
 *
 * @return std::vector<Token> A vector containing all identified tokens.
 */
    inline std::vector<Token> tokenize() {
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

                    case '[':
                        tokens.push_back(Token{TokenType::open_bracket});
                    m_index++;
                    break;

                    case ']':
                        tokens.push_back(Token{TokenType::close_bracket});
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
    };
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




private:
    /**
 * @brief Retrieves the current character from the source code string.
 *
 * This function checks if the current index is within the bounds of the
 * source code string (`m_src`). If the index is valid, it returns the
 * corresponding character; otherwise, it returns an empty optional value.
 *
 * @return std::optional<char> The character at the current index if within range, otherwise `std::nullopt`.
 */
    [[nodiscard]] inline std::optional<char> currentChar() const
    {
        if (m_index  < m_src.size()) {
            return m_src[m_index];
        }
        return {};
    }

    /**
 * @brief Appends the current character to the buffer.
 *
 * This function adds the current character from the source code string (`m_src`)
 * at the current index (`m_index`) to the buffer (`buf`). The index is then incremented.
 */
    inline void consume2buf() {
        buf.push_back(m_src[m_index++]);
    }

    /**
 * @brief The buffer which will be used to temporarily contain the reserved words found in the source code.
 *
 * This buffer is used to store the characters of reserved words (keywords) or identifiers
 * as they are read from the source code.
 */
    std::string buf;

    /**
 * @brief The source code string.
 *
 * This constant variable stores the entire source code to be tokenized and parsed by
 * the tokenizer and parser functions.
 */
    const std::string m_src;

    /**
 * @brief The index used to iterate through the source code.
 *
 * This variable keeps track of the current position in the source code string (`m_src`)
 * while processing each character during tokenization or parsing.
 */
    size_t m_index{};
};


