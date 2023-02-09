# 🟣 EvalExpr

![Stack CI workflow](https://github.com/guillaumebgd/EvalExpr/actions/workflows/main.yml/badge.svg)

This project consists in **evaluating a mathematical expression**.
It was realized as a student during the beginning of my **3rd year** at EPITECH.

### 🔎 In-depth

The complexity behind it doesn't come from the main problem itself but from the implementation in Haskell and constraints added by our instructor.

For a bit of context with the constraints:

- The project, for instance, ends up not using **that many keywords**.
`do`, `case`, `if` (and others...) aren't allowed most of the time as we will end up writing imperative code instead of functional.

- For *chaining function calls*, you will mostly see **the application operator** (`$`) and **the composition operator** (`.`) being used.

- For *conditions*, you will mostly see **pattern matching** and **guards** being used.

- **Majority of types** used as **parameters** and **return types** of functions are newly created types (via `type`, `newtype` or `data`) for ease of comprehension and logic throughout the program as well as rigorousness in type conversion.

- Almost every type which uses `deriving` will use those derived instances in production. Usually people derive from `Eq` as it makes testing for those types easier in testing environment. Deriving from `Eq`, when it serves no purpose in the production environment, makes no sense. Therefore, it is being bypassed in testing by wrapping those types and creating an instance of `Eq` from those wrappers. 

### 🔨 Build the project

This project has been setup with [Stack](https://docs.haskellstack.org/en/stable/README/) and works with a [Makefile](https://en.wikipedia.org/wiki/Make_(software)) that wraps this framework with different rules.

| Command          | Result                                          |
| ---------------- | ----------------------------------------------- |
| `make`           | Builds a ```funEvalExpr``` executable.          |
| `make tests_run` | Runs unit tests.                                |
| `make clean`     | Cleans build dumps, keeping the executable.     |
| `make fclean`    | Removes all of the files created by the build.  |
| `make re`        | Calls `make fclean` and then `make`.            |

### 🚀 Launch the project

Use the newly created `funEvalExpr` binary file to launch the project.

The expression to evaluate needs to be given as argument to the program as such:
```bash
$ ./funEvalExpr "(3+1.5)*3"
```

The program outputs the result rounded down to *2 decimal places* and returns `0` upon success.

For instance:
```bash
$ ./funEvalExpr "(0.345+5)*(-2-1)/3"
-5.34
```


In case of error with the given arguments (division by 0) or with the expression itself, the program shall display the error, and exit by returning `84`.

### 🖋️ Syntax

#### Operators

The different operators the program understands are the followings:

| Operator | Description         | Binary / Unary |
| -------- | ------------------- | -------------- |
| `+`      | Plus sign           | Binary & Unary |
| `-`      | Minus sign          | Binary & Unary |
| `*`      | Multiplication sign | Binary         |
| `/`      | Division sign       | Binary         |
| `^`      | Exponent sign       | Binary         |

#### Parentheses

Parentheses are allowed to influence operation priorities.

#### Spaces

Spaces & tabulations are ignored in the expression.
