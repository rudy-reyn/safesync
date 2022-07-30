-- SafeSync.hs
import SafeSync.Client.Files
import System.Environment (getArgs)

parseArgs args
    = case args of
        [path] -> return path
        _ -> error "usage: ./Client path"

main = do
    print =<< mkFileInfoM =<< parseArgs =<< getArgs
    print =<< mkFileInfo  =<< parseArgs =<< getArgs
