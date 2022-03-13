##
## EvalExpr
## File description:
## Makefile
##

BINARY_PATH		:= $(shell stack path --local-install-root)

COVERAGE_PATH	:= $(shell stack path --local-hpc-root)

STACK_NAME		=	funEvalExpr

NAME			=	funEvalExpr

all:
	stack build --pedantic
	cp $(BINARY_PATH)/bin/$(STACK_NAME)-exe ./$(NAME)
.PHONY:	all

debug:
	stack ghci
.PHONY:	debug

clean:
	stack clean
.PHONY:	clean

fclean: clean
	stack purge
	$(RM) $(NAME)
.PHONY:	fclean

re::	fclean
re::	all
.PHONY:	re

tests_run:
	stack test --coverage
.PHONY:	tests_run
