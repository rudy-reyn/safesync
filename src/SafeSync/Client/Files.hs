-- 07/02/22
-- File.hs
module SafeSync.Client.Files where

import Data.ByteString (ByteString)
import qualified System.Posix as Posix

data FileType
    = Unsupported
    | RegularFile
    | Directory
    -- | SymLink
    -- | NamedPipe
    -- | Socket
    -- | DeviceFile
    deriving (Eq, Enum, Show)

-- instance Enum FileType where
--     toEnum 100644 = RegularFile
--     toEnum 40755 = Directory
-- --     toEnum 20666 = DeviceFile
-- --     toEnum 120755 = SymLink
-- --     toEnum 140777 = Socket
-- --     toEnum 10644 = NamedPipe
--     toEnum 0 = Unsupported
--     fromEnum RegularFile = 100644
--     fromEnum Directory = 40755
-- --     fromEnum DeviceFile = 20666
-- --     fromEnum SymLink = 120755
-- --     fromEnum Socket = 140777
-- --     fromEnum NamedPipe = 10644
--     fromEnum Unsupported = 0

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
