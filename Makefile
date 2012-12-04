LIBNAME=libbuzz.a

main: lib
	ghc -L. -lbuzz --make play.hs -o play

clean:
	rm *.o *.a *.hi play *~

buzz.o: buzz.c buzz.h

lib: buzz.o
	ar rc ${LIBNAME} buzz.o
	ranlib ${LIBNAME}
