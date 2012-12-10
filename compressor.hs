{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign hiding (unsafePerformIO, void)
import Data.Char (chr, ord)
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import System.IO
import System.Environment (getArgs)     
import Control.Monad
import Control.Concurrent

foreign import ccall safe "lzfx.h lzfx_compress"
    c_compress :: Ptr Word8 -> CUInt -> Ptr Word8 -> Ptr CUInt -> IO CInt

foreign import ccall safe "lzfx.h lzfx_decompress"
    c_decompress :: Ptr Word8 -> CUInt -> Ptr Word8 -> Ptr CUInt -> IO CInt

adapter :: (Ptr Word8 -> CUInt -> Ptr Word8 -> Ptr CUInt -> IO CInt) -> B.ByteString -> IO B.ByteString
adapter function input =
    allocaBytes chunkSize $ \inputBuffer -> do
        allocaBytes outputSpace $ \outputBuffer -> do
            allocaBytes 32 $ \outputLength -> do
                pokeArray inputBuffer unpacked
                poke outputLength (fromIntegral outputSpace)
                -- munching data
                signal <- function inputBuffer chunkSize' outputBuffer outputLength
                if signal /= 0 then error ("ERROR " ++ show signal) else return ()
                -- take it out
                outputSize <- peek outputLength
                output <- peekArray (fromIntegral outputSize) outputBuffer
                return (B.pack output) 

    where   chunkSize = B.length input
            chunkSize' = fromIntegral chunkSize
            outputSpace = if chunkSize < 10 then 10 else chunkSize * 2
            unpacked = B.unpack input

compress :: B.ByteString -> IO B.ByteString
compress = adapter c_compress


decompress :: B.ByteString -> IO B.ByteString
decompress = adapter c_decompress

reader filename = do
    B.readFile filename >>= putStrLn . show

chunk :: Int -> B.ByteString -> [B.ByteString]
chunk size string = if B.null string then [] else B.take size string : chunk size (B.drop size string)

processArgs args = 
    let [chunkSize, threadNumber, input, output] = args in
    if length args /= 4 then do
        putStrLn "mcompress <chunkSize> <threadNumber> <input> <output>"
        return (0, 0, "", "")
    else
        return (read chunkSize, read threadNumber, input, output)



dispatcher input chunks = forkIO $ mapM_ (putMVar input) chunks

compressor threadNumber input output = replicateM_ threadNumber $ forkIO . forever $ do 
    --myThreadId >>= putStrLn . show
    (a, b) <- takeMVar input
    b' <- compress b
    putMVar output (a, b')

collector chunkNumber outputFilename output exit = forkIO $ do
    replicateM chunkNumber (takeMVar output) >>= B.writeFile outputFilename . B.concat . snd . unzip . sortBy (comparing fst)
    putMVar exit True

main = 
    let 
        --(chunkSize, threadNumber, inputFilename, outputFilename) = (8192, 1, "input", "output")
    in do
    (chunkSize, threadNumber, inputFilename, outputFilename) <- getArgs >>= processArgs
    chunks      <- B.readFile inputFilename >>= return . zip [0..] . chunk chunkSize
    input       <- newEmptyMVar
    output      <- newEmptyMVar
    exit        <- newEmptyMVar
    dispatcher input chunks
    compressor threadNumber input output
    collector (length chunks) outputFilename output exit
    takeMVar exit