# 🟣 FUN - EvalExpr

![Stack CI workflow](https://github.com/EpitechPromo2024/B-FUN-500-BDX-5-1-funEvalExpr-guillaume.bogard-coquard/actions/workflows/main.yml/badge.svg)

# Description

This is a 3rd year EPITECH project which consists in evaluating a mathematical expression.

The expression to evaluate needs to be given as argument to the program as such:
```
$> ./funEvalExpr "(3+1.5)*3"
```

The program outputs the result rounded down to two decimal places and returns `0`.<br>
For instance:<br>
```
$> ./funEvalExpr "(0.345+5)*(-2-1)/3"
-5.34
```

* Parentheses are allowed to influence operation priorities.<br>
* Spaces & tabulations are ignored in the expression.

In case of error with the given arguments or with the expression itself, the program shall print the error, and exit by returning `84`.

## Operators

The different operators the program understands are the followings:

| Operator | Description         |
| -------- | ------------------- |
| `+`      | Plus sign           |
| `-`      | Minus sign          |
| `*`      | Multiplication sign |
| `/`      | Division sign       |
| `^`      | Exponent sign       |

# How to use the project ? 🔥

This project has been setup with [Stack](https://docs.haskellstack.org/en/stable/README/) and works with a Makefile that wraps this framework with different rules.

| Command          | Result                                          |
| ---------------- | ----------------------------------------------- |
| `make`           | Builds a ```funEvalExpr``` executable.          |
| `make tests_run` | Runs unit tests.                                |
| `make clean`     | Cleans build dumps, keeping the executable.     |
| `make fclean`    | Removes all of the files created by the build.  |
| `make re`        | Calls `make fclean` and then `make`.            |
