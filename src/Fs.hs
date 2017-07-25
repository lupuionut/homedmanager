module Fs where

import qualified System.Directory as Directory
import qualified System.IO as I

-- | retrieve the configuration directory path
getCfgDirectory :: IO FilePath
getCfgDirectory =
    do
        xdg <- Directory.getXdgDirectory Directory.XdgConfig ""
        return $ xdg ++ "/homedmanager"


-- | create the configuration directory
-- | /homedir/.config/homedmanager
createCfgDirectory :: Bool -> FilePath -> IO ()
createCfgDirectory False d = return ()
createCfgDirectory True d = Directory.createDirectory d


-- | alias for System.Directory.doesDirectoryExist
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = Directory.doesDirectoryExist


storeInFile :: FilePath -> String -> IO ()
storeInFile f s = writeFile f s


loadFromFile :: FilePath -> IO String
loadFromFile = readFile
