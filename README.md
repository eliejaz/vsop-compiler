# VSOP Compiler

## Overview

This report provides a broad overview of our compiler implementation for the VSOP language. It details the tools used, the organization of the code, the responsibilities of each module, the data structures and algorithms employed, and the decisions made throughout the implementation. It also addresses conflicts, limitations, and possible improvements.

## Tools Used

- **Flex**: Used for lexical analysis to tokenize the input source code.
- **Bison**: Used for parsing to generate the syntax tree.
- **LLVM**: Used for intermediate representation (IR) and code generation.
- **C++**: The primary language for implementing the compiler.

## Organization

The compiler is organized into several modules, each responsible for a specific task:

### 1. Lexical Analysis (Lexer)
- **Tool**: Flex
- **Responsibility**: Tokenizing the input source code into a stream of tokens.

### 2. Syntax Analysis (Parser)
- **Tool**: Bison
- **Responsibility**: Constructing the Abstract Syntax Tree (AST) from the token stream.

### 3. Semantic Analysis
- **Responsibility**: Ensuring the correctness of the program by checking types, scope, and other semantic rules.

### 4. Intermediate Representation and Code Generation
- **Tool**: LLVM
- **Responsibility**: Converting the AST into LLVM IR and generating the final machine code.

## Key Modules and Responsibilities

- **ASTClasses.hpp**: Defines the structure of the AST nodes.
- **ASTClassesSemanticChecker.cpp**: Implements the semantic checks for AST nodes.
- **ASTClassesCodeGenerator.cpp**: Implements the code generation logic for AST nodes.
- **ClassSymbolTable.hpp**: Manages symbol tables for classes and functions.
- **CodeGenerator.hpp**: Contains definitions for the code generation process.
- **ProgramScope.hpp**: Manages the program scope and nested scopes.
- **lexer.lex**: Defines the lexical analysis rules and token definitions.
- **parser.y**: Defines the grammar rules for parsing VSOP and constructing the AST.
- **main.cpp**: Main function that drives the compilation process.

## Data Structures and Algorithms

- **Abstract Syntax Tree (AST)**: A hierarchical tree structure representing the parsed program.
- **Symbol Table**: Stores information about identifiers, their types, and scope levels.
- **Program Scope**: Manages scopes in the program. It uses a map to store symbol-to-type mappings and supports nested scopes via a parent pointer.

## Compilation and Execution

### To compile:
```bash
make
```

### To clean build files:
```bash
make clean
```

### To run lexical analysis:
```bash
./vsopc -l [file_name]
```

### To run syntax analysis:
```bash
./vsopc -p [file_name]
```

### To run semantic analysis:
```bash
./vsopc -c [file_name]
```

### To compile the code to LLVM files:
```bash
./vsopc -i [file_name]
```

### To compile the code to binary:
```bash
./vsopc [file_name]
```

For more details, check `vsop_manual.pdf` and `compiler_report.pdf`.
