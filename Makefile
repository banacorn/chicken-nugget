LIBNAME=libbuzz.a

main:
	ghc --make -main-is main compressor.hs lzfx.c

clean:
	rm lzfx.o compressor.hi compressor.o

test:
	./compressor 123 10 input output
