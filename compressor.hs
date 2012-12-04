{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign hiding (unsafePerformIO, void)
import Data.Char (chr, ord)
import Data.Ord (comparing)
import Data.List (sortBy)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)     
import Control.Monad
import Control.Concurrent

foreign import ccall "lzfx.h lzfx_compress"
    c_compress :: Ptr CUChar -> CUInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

foreign import ccall "lzfx.h lzfx_decompress"
    c_decompress :: Ptr CUChar -> CUInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

adapter :: (Ptr CUChar -> CUInt -> Ptr CUChar -> Ptr CUInt -> IO CInt) -> Int -> String -> String
adapter function chunkSize content = map toChar $ unsafePerformIO $ do
    -- allocate buffers
    inputBuffer <- mallocArray chunkSize
    outputBuffer <- mallocArray chunkSize
    outputSize <- mallocBytes 32
    -- put it in
    pokeArray inputBuffer (map fromChar content)
    poke outputSize (fromIntegral chunkSize)
    -- compress
    function inputBuffer (fromIntegral chunkSize) outputBuffer outputSize
    -- take it out
    output <- peekArray chunkSize outputBuffer
    -- free buffers
    free inputBuffer
    free outputBuffer
    free outputSize
    return output
    where   toChar = chr . fromIntegral
            fromChar = fromIntegral . ord

compress :: Int -> String -> String
compress = adapter c_compress

decompress :: Int -> String -> String
decompress = adapter c_decompress

readInputFile :: FilePath -> IO String
readInputFile filename = openBinaryFile filename ReadMode >>= hGetContents


chunk size [] = []
chunk size string = a : chunk size rest
    where (a, rest) = splitAt size string

main =
    let 
        threadNumber = 10
        chunkNumber = 50
        chunks = zip [0..49] [1..50]
    in do
    (chunkSize, threadNumber, inputFilename, outputFilename) <- getArgs >>= processArgs
    exit <- newEmptyMVar
    input <- newEmptyMVar
    output <- newEmptyMVar
    dispatcher input chunks
    compressor threadNumber input output
    collector chunkNumber output exit
    takeMVar exit
    putStrLn "hello"

processArgs args = 
    let [chunkSize, threadNumber, input, output] = args in
    if length args /= 4 then do
        putStrLn "mcompress <chunkSize> <threadNumber> <input> <output>"
        return (0, 0, "", "")
    else
        return (read chunkSize, read threadNumber, input, output)


compressor threadNumber input output = replicateM_ threadNumber . forkIO . forever $ takeMVar input >>= putMVar output . processData
    where processData (a, b) = (a, b * 2)

dispatcher input chunks = forkIO $ mapM_ (putMVar input) chunks

--collector :: Int -> MVar Int -> MVar Bool -> IO ThreadId
collector chunkNumber output exit = forkIO $ do
    replicateM chunkNumber (takeMVar output) >>= putStrLn . show . snd . unzip . sortBy (comparing fst) >> putMVar exit True
