LIBNAME=libbuzz.a

main:
	ghc -threaded compressor.hs lzfx.c
	# ghc -prof -auto-all -threaded compressor.hs lzfx.c
	# ghc -threaded test.hs lzfx.c

clean:
	rm *.o *.hi

run:
	time ./compressor +RTS -N -RTS 4096 32 input output
	# time ./compressor +RTS -N -RTS 4096 1 input output

prof:
	ghc -prof -auto-all -threaded compressor.hs lzfx.c
	time ./compressor +RTS -N -p -RTS 4096 32 input output
	
	# time ./compressor +RTS -RTS < data.csv > /dev/null
	# time ./compressor 8192 32 input output


	# ghc -threaded -prof -auto-all test.hs
	# time ./test +RTS -p -RTS < data.csv > /dev/null


bana:
	ghc -threaded test.hs bread.c
	time ./test

