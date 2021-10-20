##
## EPITECH PROJECT, 2021
## makefile
## File description:
## makefile
##

STACK_PATH := $(shell stack path --local-install-root)

NAME = funEvalExpr

all:
	stack build
	cp $(STACK_PATH)/bin/funEvalExpr-exe ./$(NAME)

tests_run:
	stack test

clean:
	stack clean

fclean:	clean
	$(RM) $(NAME)

re:	fclean all

.PHONY: all clean fclean re