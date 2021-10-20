##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile
##

STACK_PATH := $(shell stack path --local-install-root)

NAME = funEvalExpr

MAIN    := app/Main.hs

SRC        := src/EvalExpr.hs
SRC        += src/Parse.hs

TESTSRC := test/Spec.hs

OBJ := $(MAIN:.hs=.o) $(MAIN:.hs=.hi)
OBJ += $(SRC:.hs=.o) $(SRC:.hs=.hi)
OBJ += $(TESTSRC:.hs=.o) $(TESTSRC:.hs=.hi)


all: $(SRC) $(MAIN)
	stack build
	cp $(STACK_PATH)/bin/funEvalExpr-exe ./$(NAME)

tests_run:
	stack test

clean:
	stack clean

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all tests_run clean fclean re