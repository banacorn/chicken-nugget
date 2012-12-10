import Banana
import qualified Data.ByteString as B
import System.Environment (getArgs)

main = do
	[input, output] <- getArgs
	B.readFile input >>= decompress >>= B.writeFile output