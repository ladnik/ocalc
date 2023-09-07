# OCalc 🐫
OCalc is a simple program for parsing and evaluating arithmetic expressions like "1+2*3".
The calculator works on float values for in- and output.

Supported operations: addition, subtraction, multiplication, division, modulus and exponentiation.

Not supported: unary operations such as `-, !` and logical operations such as `AND, OR, ...`, as well as other useful functions like `sin, cos, sqrt, ...`

If you have any questions regarding this project or ideas on improving it, feel free to contact me.

## Implementation
As the name suggests, OCalc is implemented in the functional programming language OCaml.
The algorithm itself consists of three parts:
### I. Lexer
The lexer performs a lexical analysis of the input string i.e. it converts the input into 
a list of tokens and detects syntactically erroneous inputs (indicated by an 'Invalid' token).

It recognizes the following symbols:

- Binary operators: `+, -, *, /, %, ^`
- Parentheses: `(, )`
- Values: floating point constants

(`%` denotes the modulus operator, `^` exponentiation)

### II. Parser
The parser takes the output token list of the lexer and converts it to an abstract syntax tree (AST)
whilst respecting operator precedence. This is achieved by using a recursive descent parser. [[Wikipedia]](https://en.wikipedia.org/wiki/Recursive_descent_parser) [[craftinginterpreters]](http://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing). A function to convert the AST to its graphviz representation (viewable with [Webgraphviz](http://webgraphviz.com/) is provided.

### III. Evaluator
Evaluation of the generated abstract syntax tree is the simplest part, it only consists in 
recursively evaluating all subtrees by replacing the arithmetic operation nodes by their
respective function (+), (-), etc. Evaluation will performed with floats.

## Usage
### UTop
To run the program in the toplevel [utop](https://github.com/ocaml-community/utop), nagivate to the source directory `ocalc/` and execute `dune utop`.
In the toplevel, run  `#use "calc.ml"`. After that the following commands are available:
- `generate_token_list`: Generates the list of lexer tokens from a given string. Signature: string -> float token list
- `generate_syntax_tree`: Generates the AST from a list of lexer tokens. Signature: float token list -> float expr
- `eval_syntax_tree`: Evaluates a given AST. Signature: float expr -> float
- `eval_string_expr`: Evaluates a given string expression, combines the three previously listed commands. Signature: string -> float
- `print_ast_to_file`: Prints an AST in the graphviz format to the specified file. Signature: string -> float expr -> unit

### Dune
With the build system [dune](https://github.com/ocaml/dune) installed, an executable can be built from the source as follows:
Navigate to the source directory `ocalc/` and execute `dune build calc.exe`. The executable can be run with `dune exec _build/default/calc.exe`.

### OCaml compiler
To compile the program to a binary, use the ocamlopt compiler:
- Make sure your ocaml libraries are up to date by running `opam update`, `opam upgrade` and `eval $(opam env)`.
- Build the modules: `ocamlopt -c lexer/lexer.ml && ocamlopt -c -I lexer parser/parser.ml && ocamlopt -c -I lexer -I parser -I $(opam var lib)/ounit2 calc.ml`
- Link the modules: `ocamlfind ocamlopt -o out -package ounit2 -linkpkg lexer/lexer.cmx parser/parser.cmx calc.cmx`
- Run the program: `./out`

A pre-built executable can be found here. (TODO) 

## Examples
- "1+2+3" -> 6.0
- "1+2-3" -> 0.0
- "2/3*3" -> 2.0
- ".0+.1" -> 0.1
- "1.+.1" -> 1.1
- "2^(2-2)" -> 1.0
- "1+1/1+1/2+1/(1\*2\*3)+1/(1\*2\*3\*4)+1/(1\*2\*3\*4\*5)" -> 2.71666666667
- "5%(2*2)" -> 1.0
