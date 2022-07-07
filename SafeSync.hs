-- 07/03/22
-- SafeSync.hs
import SafeSync.Files
import System.Environment (getArgs)

parseArgs args
    = case args of
        [path] -> return path
        _ -> error "usage: ./Monitor path"

main = do
    print =<< mkFileInfoM =<< parseArgs =<< getArgs
    print =<< mkFileInfo  =<< parseArgs =<< getArgs
