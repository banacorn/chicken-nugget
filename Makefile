input = input
output = output

main: compressor.hs banana.hs decompressor.hs lzfx.c lzfx.h
	ghc -threaded -with-rtsopts="-N" -o mcompress compressor.hs banana.hs lzfx.c
	ghc decompressor.hs lzfx.c

benchmark:
	@echo "chunksize 1024, thread number 1 ~ 8"
	time ./mcompress 1024 1 ${input} ${output}
	time ./mcompress 1024 2 ${input} ${output}
	time ./mcompress 1024 4 ${input} ${output}
	time ./mcompress 1024 8 ${input} ${output}
	@echo "chunksize 2048, thread number 1 ~ 8"
	time ./mcompress 2048 1 ${input} ${output}
	time ./mcompress 2048 2 ${input} ${output}
	time ./mcompress 2048 4 ${input} ${output}
	time ./mcompress 2048 8 ${input} ${output}
	@echo "chunksize 4096, thread number 1 ~ 8"
	time ./mcompress 4096 1 ${input} ${output}
	time ./mcompress 4096 2 ${input} ${output}
	time ./mcompress 4096 4 ${input} ${output}
	time ./mcompress 4096 8 ${input} ${output}
	@echo "chunksize 8192, thread number 1 ~ 8"
	time ./mcompress 8192 1 ${input} ${output}
	time ./mcompress 8192 2 ${input} ${output}
	time ./mcompress 8192 4 ${input} ${output}
	time ./mcompress 8192 8 ${input} ${output}



# for profiling
prof:
	ghc -prof -auto-all -with-rtsopts="-N -p" -threaded compressor.hs lzfx.c
	time ./mcompress 4096 4 ${input} ${output}


clean:
	rm -rf *.o *.hi
