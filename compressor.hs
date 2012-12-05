{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign hiding (unsafePerformIO, void)
--import Data.Char (chr, ord)
--import Data.Ord (comparing)
--import Data.List (sortBy)
import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)
--import System.Environment (getArgs)     
--import Control.Monad
--import Control.Concurrent

foreign import ccall "lzfx.h lzfx_compress"
    c_compress :: Ptr CUChar -> CUInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

foreign import ccall "lzfx.h lzfx_decompress"
    c_decompress :: Ptr CUChar -> CUInt -> Ptr CUChar -> Ptr CUInt -> IO CInt

compress :: Int -> B.ByteString -> B.ByteString
compress chunkSize input = unsafePerformIO $ do
    return $ B.pack [1..4]


reader filename = do
    B.readFile filename >>= putStrLn . show

chunk :: Int -> B.ByteString -> [B.ByteString]
chunk size string = if B.null string then [] else B.take size string : chunk size (B.drop size string)


a = B.pack [1..10]

main = putStrLn $ show $ chunk 3 a

