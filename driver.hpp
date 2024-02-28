/* This flex/bison example is provided to you as a starting point for your
 * assignment. You are free to use its code in your project.
 *
 * This example implements a simple calculator. You can use the '-l' flag to
 * list all the tokens found in the source file, and the '-p' flag (or no flag)
 * to parse the file and to compute the result.
 *
 * Also, if you have any suggestions for improvements, please let us know.
 */

#ifndef _DRIVER_HPP
#define _DRIVER_HPP

#include <string>
#include <vector>
#include <map>

#include "parser.hpp"

// Give prototype of yylex() function, then declare it.
#define YY_DECL VSOP::Parser::symbol_type yylex()
YY_DECL;

namespace VSOP
{
    class Driver
    {
    public:
        /**
         * @brief Construct a new Driver.
         *
         * @param _source_file The file containing the source code.
         */
        Driver(const std::string &_source_file) : source_file(_source_file) {}

        /**
         * @brief Get the source file.
         *
         * @return const std::string& The source file.
         */
        const std::string &get_source_file() { return source_file; }

        /**
         * @brief Add a new integer variable.
         *
         * @param name The name of the variable.
         * @param value The value of the variable.
         */
        void add_variable(std::string name, int value) { variables[name] = value; }

        /**
         * @brief Check if a variable exists.
         *
         * @param name The name of the variable.
         *
         * @return true If the variable exists.
         * @return false If the variable does not exist.
         */
        bool has_variable(std::string name) { return variables.count(name); }

        /**
         * @brief Get the interger value of a variable.
         *
         * @param name The name of the variable.
         *
         * @return int The value of the variable.
         */
        int get_variable(std::string name) { return variables.at(name); }

        /**
         * @brief Run the lexer on the source file.
         *
         * @return int 0 if no lexical error.
         */
        int lex();

        /**
         * @brief Run the parser on the source file and compute the result.
         *
         * @return int 0 if no syntax or lexical error.
         */
        int parse();

        /**
         * @brief Print all the tokens.
         */
        void print_tokens();

        /**
         * @brief The result of the computation.
         */
        int result;

    private:
        /**
         * @brief The source file.
         */
        std::string source_file;

        /**
         * @brief The parser.
         */
        VSOP::Parser *parser;

        /**
         * @brief Store the variables (names + values).
         */
        std::map<std::string, int> variables;

        /**
         * @brief Store the tokens.
         */
        std::vector<Parser::symbol_type> tokens;

        /**
         * @brief Start the lexer.
         */
        void scan_begin();

        /**
         * @brief Stop the lexer.
         */
        void scan_end();
    };
}

#endif
