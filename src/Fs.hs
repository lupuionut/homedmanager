module Fs where

import qualified System.Directory as D
import qualified System.IO as I

-- | retrieve the configuration directory path
getCfgDirectory :: IO FilePath
getCfgDirectory =
    do
        xdg <- D.getXdgDirectory D.XdgConfig ""
        return $ xdg ++ "/homedmanager"


-- | create the configuration directory
-- | /homedir/.config/homedmanager
createCfgDirectory :: Bool -> FilePath -> IO ()
createCfgDirectory False d = return ()
createCfgDirectory True d = D.createDirectory d


createCacheFile :: Bool -> FilePath -> IO ()
createCacheFile False _ = return ()
createCacheFile True f = storeInFile f ""


-- | check for existance of .config/homedmanager
-- | if doesn't exist, it creates it
-- | this directory will store the auth keys
mkOrRetStorageDir :: IO FilePath
mkOrRetStorageDir =
    do
        cfg <- Fs.getCfgDirectory
        Fs.doesDirectoryExist cfg >>= (\b -> Fs.createCfgDirectory (not b) cfg)
        return cfg


mkOrRetTokenCacheFile :: IO FilePath
mkOrRetTokenCacheFile =
    do
        storage <- mkOrRetStorageDir
        let cachefile = storage ++ "/tokens.cache"
        D.doesFileExist cachefile >>= (\b -> createCacheFile (not b) cachefile)
        return cachefile


-- | alias for System.Directory.doesDirectoryExist
doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = D.doesDirectoryExist


doesFileExist :: FilePath -> IO Bool
doesFileExist = D.doesFileExist

storeInFile :: FilePath -> String -> IO ()
storeInFile f s = writeFile f s


loadFromFile :: FilePath -> IO String
loadFromFile = readFile
