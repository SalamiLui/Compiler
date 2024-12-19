#include <iostream>
#include <fstream>
#include <optional>
#include <sstream>
#include <vector>

#include "./tokenization.hpp"
#include "parser.hpp"
#include "generation.hpp"


int main(int argc, char *argv[]) {
    if (argc != 2) {
        std::cerr << "Incorrect usage. Correct usage is..."<<std::endl;
        std::cerr << "NaziScrpt <input.nzi>"<<std::endl;
        return EXIT_FAILURE;
    }


    std::string contents;
    {
        std::fstream input(argv[1], std::ios::in);
        std::stringstream content_stream;
        content_stream << input.rdbuf();
        contents = content_stream.str();
    }

    Tokenizer tokenizer(contents);
    std::vector<Token> tokens =  tokenizer.tokenize();

    Parser parser(tokens);
    NodeProg tree = parser.parse_prog();

    Generator generator(tree);

    {
        std::fstream file("../out.asm", std::ios::out);
        file << generator.generate();
    }

    return EXIT_SUCCESS;
}
