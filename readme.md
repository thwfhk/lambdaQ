# λQ

**λQ**: A Simple Quantum Programming Language based on [QWIRE](https://github.com/inQWIRE/QWIRE) with a compiler to [QASM](https://github.com/Qiskit/openqasm).

The name λQ means *lambda calculus with quantum circuits*.

This is a term project of the course *Compiler Principles* of Peking University.

## Language Specification

`specification/specification.pdf`

## Frontend

`src/Frontend`

### Files
Syntax Definition:
- λQ syntax : `src/Frontend/src/Syntax.hs`
- variable binding context : `src/Frontend/src/Context.hs`
- syntax sugar : `src/Frontend/src/Desugar.hs`

Lexer & Parser:
- lexer : `src/Frontend/src/Lexer.hs`
- parser : `src/Frontend/src/Parser.hs`

Type Inference:
- type inference : `src/Frontend/src/TypeChecker.hs`

Code Generation:
- QASM (IR) syntax : `src/Frontend/src/QASMSyntax.hs`
- code generation : `src/Frontend/src/CodeGenerator.hs`

Printer:
- printer : `src/Frontend/src/PrettyPrinter.hs`


## Backend

`src/Backend`

## Reference