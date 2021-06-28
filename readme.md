# **λQ**: A Simple Quantum Programming Language

(Work In Progress)

λQ is a simple quantum programming language based on [QWIRE](https://github.com/inQWIRE/QWIRE) with a compiler to [QASM](https://github.com/Qiskit/openqasm).

The name λQ means ***lambda calculus with quantum circuits***.

This is a term project of the course *Compiler Principles* of Peking University.


## Report
- [slides: `slides.pdf`](./LambdaQ-slides.pdf)
- [report: `report/report.pdf`](report/report.pdf)

## Usage
### Build
- Frontend: run `stack build` in the directory `src/Frontend`. The output executable file is in `.stack-work/install/xxxx/bin`.
- Backend: run `makefile` in ...

### Run
- `frontend-exe input-file output-file`
- `backend`

## Code Structure
### Frontend

[`src/Frontend`](src/Frontend/)

#### Syntax Definition:
- λQ syntax : `src/Frontend/src/Syntax.hs`
- variable binding context : `src/Frontend/src/Context.hs`
- syntax sugar : `src/Frontend/src/Desugar.hs`

#### Lexer & Parser:
- lexer : `src/Frontend/src/Lexer.hs`
- parser : `src/Frontend/src/Parser.hs`

#### Type Inference:
- type inference : `src/Frontend/src/TypeChecker.hs`

#### Code Generation:
- QASM (IR) syntax : `src/Frontend/src/QASMSyntax.hs`
- code generation : `src/Frontend/src/CodeGenerator.hs`

#### Printer:
- printer : `src/Frontend/src/PrettyPrinter.hs`


### Backend
[`src/Backend`](src/Backend)

#### Lexer & Parser
- lexer : `src/Backend/src/lex.l`
- parser : `src/Backend/src/parser.y`

#### Qubit Allocation
- qubit allocation : `src/Backend/src/qubit_allocation.cpp`

#### Optimization
- graph conversion : `src/Backend/src/generator.cpp`
- optimization: `src/Backend/src/optimization.cpp`

#### Code Generation
- code generation from AST : `src/Backend/src/generation.cpp`
- code generation from graph : `src/Backend/src/graph.h`   


## References
- Cross, A. W., Bishop, L. S., Smolin, J. A., & Gambetta, J. M. (2017). Open quantum assembly language. arXiv preprint arXiv:1707.03429.
- Paykin, J., Rand, R., & Zdancewic, S. (2017, January). QWIRE: a core language for quantum circuits. In Proceedings of the 44th ACM SIGPLAN Symposium on Principles of Programming Languages (pp. 846-858).
- Rand, R., Paykin, J., & Zdancewic, S. (2018). QWIRE practice: Formal verification of quantum circuits in Coq. arXiv preprint arXiv:1803.00699.
- Nam, Y., Ross, N.J., Su, Y. et al. Automated optimization of large quantum circuits with continuous parameters. npj Quantum Inf 4, 23 (2018). https://doi.org/10.1038/s41534-018-0072-4
- Marcos Yukio Siraichi, Vinícius Fernandes dos Santos, Sylvain Collange, and Fernando Magno Quintao Pereira. 2018. Qubit allocation. In Proceedings of the 2018 International Symposium on Code Generation and Optimization (CGO 2018). Association for Computing Machinery, New York, NY, USA, 113–125. DOI:https://doi.org/10.1145/3168822
