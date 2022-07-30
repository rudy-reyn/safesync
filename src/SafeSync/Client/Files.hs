-- 07/02/22
-- File.hs
module SafeSync.Client.Files where

import Data.ByteString (ByteString)
import qualified System.Posix as Posix

data FileType
    = Unsupported
    | RegularFile
    | Directory
    deriving (Eq, Enum, Show)

data FileInfo
    = FileInfo {
            filePath :: String,
            fileType :: FileType,
            fileSize :: Posix.FileOffset,
            modificationTime :: Posix.EpochTime
        }
    deriving (Eq, Show)

getFileType :: Posix.FileStatus -> IO FileType
getFileType stat
    | Posix.isRegularFile stat = return RegularFile
    | Posix.isDirectory stat = return Directory
    | otherwise = return Unsupported

mkFileInfo' :: String -> Posix.FileStatus -> IO FileInfo
mkFileInfo' path stat = do
    ftype <- getFileType stat
    return $
        FileInfo {
            filePath = path,
            fileType = ftype,
            fileSize = Posix.fileSize stat,
            modificationTime = Posix.modificationTime stat
        }

mkFileInfo :: String -> IO FileInfo
mkFileInfo path = mkFileInfo' path =<< Posix.getFileStatus path

mkFileInfoM path = do
    exists <- Posix.fileExist path
    if not exists
    then return Nothing
    else do fileInfo <- mkFileInfo path
            return $ Just fileInfo
