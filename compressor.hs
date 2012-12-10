{-# LANGUAGE ForeignFunctionInterface #-}

import Banana
--import Foreign.C
--import Foreign hiding (unsafePerformIO, void)
import Data.Char (chr, ord)
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.ByteString as B
--import qualified Data.ByteString.Internal as BI
import System.IO
import System.Environment (getArgs)     
import Control.Monad
import Control.Concurrent
import Control.Applicative ((<$>), (<*>))

type ChunkSize = Int
type ThreadNumber = Int
type Chunk = (Int, B.ByteString)

-- munch args
processArgs :: [String] -> IO (ChunkSize, ThreadNumber, FilePath, FilePath)
processArgs args = 
    let [chunkSize, threadNumber, input, output] = args in
    if length args /= 4 then do
        putStrLn "mcompress <chunkSize> <threadNumber> <input> <output>"
        return (0, 0, "", "")
    else
        return (read chunkSize, read threadNumber, input, output)

-- producer
-- puts all chunks in the MVar `input`
dispatcher :: MVar Chunk -> [Chunk] -> IO ThreadId
dispatcher input chunks = forkIO $ mapM_ (putMVar input) chunks

--decompressor output input = B.readFile output >>= decompress >>=  B.writeFile (input ++ "_")

-- processor, with specific number of worker threads
-- takes a chunk from `input`, process it, and puts it back to `output`
compressor :: ThreadNumber -> MVar Chunk -> MVar Chunk -> IO ()
compressor threadNumber input output = replicateM_ threadNumber $ forkIO . forever $ do 
    (a, b) <- takeMVar input
    b' <- compress b
    putMVar output (a, b')

-- consumer
-- collects all proccesed chunks from `input`, rip order-tags, concat and write file
-- put unit in MVar `exit` to notify the main thread to exit
collector :: Int -> FilePath -> MVar Chunk -> MVar () -> IO ThreadId
collector chunkNumber outputFilename output exit = forkIO $ do
    replicateM chunkNumber (takeMVar output) >>= B.writeFile outputFilename . B.concat . snd . unzip . sortBy (comparing fst)
    putMVar exit ()

main = do
    (chunkSize, threadNumber, inputFilename, outputFilename) <- getArgs >>= processArgs
    chunks      <- B.readFile inputFilename >>= tagChunks chunkSize
    
    -- MVars
    input       <- newEmptyMVar
    output      <- newEmptyMVar
    exit        <- newEmptyMVar

    -- 3-stages
    dispatcher input chunks
    compressor threadNumber input output
    collector (length chunks) outputFilename output exit
    
    takeMVar exit   -- blocks when not done

    where tagChunks chunkSize = return . zip [0..] . chunk chunkSize