LIBNAME=libbuzz.a

main:
	ghc -prof -auto-all --make -O -main-is main compressor.hs lzfx.c

clean:
	rm lzfx.o compressor.hi compressor.o

test:
	time ./compressor +RTS -p -RTS < data.csv > /dev/null
	# time ./compressor 8192 32 input output
