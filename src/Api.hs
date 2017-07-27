module Api where

type Token = String
type CommandList = Maybe[String]

execute :: CommandList -> Token -> IO ()
execute Nothing _ = return ()
execute (Just commandList) withToken = do
    let command = unwords commandList
    putStrLn command
