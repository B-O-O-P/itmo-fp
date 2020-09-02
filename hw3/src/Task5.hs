module Task5
    ( FS(..)
    , scanFS
    , name
    , contents
    , _File
    , _Dir
    ) where


import Control.Exception (throwIO)
import Lens.Micro (Lens', lens, Traversal')
import System.Directory
       (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, splitPath, (</>))


import Utils.FSException

-- | Data type, which represents simple tree of file system: Directories and Files.
-- Directory consists of its name and contents, file consists of name.
data FS
    = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    }
  deriving (Eq, Show)

-- | Scan directory by given path and creates its representation in 'FS' type.
-- * 'PathDoesNotExists'
--    Is thrown if there is no such directory by given path.
scanFS :: FilePath -> IO FS
scanFS path = do
  isFile <- doesFileExist path
  isDirectory <- doesDirectoryExist path
  if isFile
    then return $ File (takeFileName path)
  else if isDirectory
    then do
      content <- listDirectory path
      contentFS <- mapM (\fs -> scanFS (path </> fs)) content
      return $ Dir (last . splitPath $ path) contentFS
    else throwIO PathDoesNotExists

-- | Lens for name field of 'FS' element.
name :: Lens' FS FilePath
name = lens _name (\fs path -> fs { _name = path })

-- | Lens for contents field of 'FS' directory. Uses '_Dir' prism.
contents :: Traversal' FS [FS]
contents = _Dir . lens _contents (\fs path -> fs { _contents = path })

-- | Prism for files, which is just traversal that target 0 or 1 file.
_File :: Traversal' FS FS
_File f fs@(File _) = id <$> f fs
_File _ other = pure other

-- | Prism for directories, which is just traversal that target 0 or 1 directory.
_Dir :: Traversal' FS FS
_Dir f fs@(Dir _ _) = id <$> f fs
_Dir _ other = pure other
