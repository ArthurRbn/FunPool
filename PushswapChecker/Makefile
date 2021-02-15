##
## EPITECH PROJECT, 2019
## Makefile
## File description:
## Makefile
##

SRC		=		DoOp.hs				\
				Op.hs				\
				PushswapChecker.hs	\
				Main.hs

OBJ		=		$(SRC:.hs=.hs)

NAME	=		pushswap_checker

all:	$(NAME)

$(NAME):		$(OBJ)
	ghc -Wall $(OBJ) -o $(NAME)

clean:
	rm -f *.hi
	rm -f *.o

fclean: clean
	rm -f $(NAME)

re: fclean all