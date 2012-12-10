module Banana (
	compress,
	decompress,
	chunk
) where

import Foreign
import Foreign.C
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI

-- FFI FTW
foreign import ccall safe "lzfx.h lzfx_compress"
    c_compress :: Ptr Word8 -> Int -> Ptr Word8 -> Ptr Int -> IO Int

foreign import ccall safe "lzfx.h lzfx_decompress"
    c_decompress :: Ptr Word8 -> Int -> Ptr Word8 -> Ptr Int -> IO Int

-- accept a marshalled foreign function, and return a function goes ByteString -> IO ByteString
adapter :: (Ptr Word8 -> Int -> Ptr Word8 -> Ptr Int -> IO Int) -> B.ByteString -> IO B.ByteString
adapter function input = do
    -- make bytestring as input buffer
    withForeignPtr pointer $ \inputPtr ->
        let inputBuffer = inputPtr `plusPtr` offset in

        -- allocate spaces for output buffer & output length
        allocaBytes outputSize $ \outputBuffer -> do
            allocaBytes 32 $ \outputSizePtr -> do

                -- put size of output buffer in
                poke outputSizePtr outputSize

                -- munching data
                signal <- function inputBuffer size outputBuffer outputSizePtr
                --putStrLn . show $ signal
                if signal == -2 then error "fuck -2" else return ()
                handle signal

                -- take size of new output buffer out
                outputNewSize <- peek outputSizePtr

                -- FUCK IT, FUCK!
                -- correct but slower
                output <- peekArray outputNewSize outputBuffer
                return $ B.pack output


	                -- make output buffer a bytestring and lift it
	                --outputBufferForeignPtr <- newForeignPtr_ outputBuffer
	                --return $ BI.fromForeignPtr outputBufferForeignPtr 0 outputNewSize -- fromForeignPtr :: ForeignPtr -> Offset -> Length

        where   -- destruct ByteString for direct accessing
                (pointer, offset, size) = BI.toForeignPtr input

                -- the output may be bigger than the input
                -- lower bound: 10
                outputSize = if size < 10 then 10 else size * 30
                
                -- signal handler
                handle -1 = error "Output buffer too small"
                handle -2 = error "Invalid data for decompression"
                handle -3 = error "Arguments invalid (NULL)"
                handle _ = return ()


-- adapted compress/decompress function
compress :: B.ByteString -> IO B.ByteString
compress = adapter c_compress

decompress :: B.ByteString -> IO B.ByteString
decompress = adapter c_decompress
--decompress input = do
--	allocaBytes chunkSize $ \inputBuffer -> do
--        allocaBytes outputSpace $ \outputBuffer -> do
--            allocaBytes 32 $ \outputLength -> do
--                pokeArray inputBuffer unpacked
--                poke outputLength outputSpace
--                -- munching data
--                signal <- c_decompress inputBuffer chunkSize outputBuffer outputLength
--                if signal /= 0 then error ("ERROR " ++ show signal) else return ()
--                -- take it out
--                outputSize <- peek outputLength
--                output <- peekArray outputSize outputBuffer
--                return (B.pack output) 


--    where   chunkSize = B.length input
--            outputSpace = if chunkSize < 10 then 10 else chunkSize * 2
--            unpacked = B.unpack input


-- chunk ByteString
chunk :: Int -> B.ByteString -> [B.ByteString]
chunk size string = if B.null string then [] else B.take size string : chunk size (B.drop size string)
