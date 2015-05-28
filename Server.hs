{- Alec Snyder
- lab 2 chat server program
- following networking from O'sullivan's Real World Haskell
-}
module Main (main) where

import Network
--import Network.Socket
import Network.BSD
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Environment

import Chat

--Main: Set up socket and start forking
main :: IO ()
main = withSocketsDo $ do
    portStr <- getEnv "CHAT_SERVER_PORT"
    sock <- listenOn $ Service portStr
    putStrLn $ "Listening on port " ++ portStr
    
    var <- newMVar []
    acceptCons sock var

--acceptCons: loops and accepts connections and then spawns child procs
--to handle clients

acceptCons :: Socket -> MVar [User] -> IO ()
acceptCons sock var = do
    (handle,connHost, clientPort) <- accept sock
    putStrLn $ "Client connected from: " ++ connHost
    forkIO (handleClient handle var)
    acceptCons sock var

{-
handleClient: takes in the new sockets and 
initializes the user in the user list and prints instructions
to the user. It then passes the socket off 
to the text processing function chat 
-}

handleClient :: Handle -> MVar [User] -> IO ()
handleClient hand var= do
    hSetNewlineMode hand (NewlineMode CRLF CRLF)
    hSetBuffering hand LineBuffering
    introduce hand
    newUser <- joinUser hand var
    chat newUser var

chat :: User -> MVar [User] -> IO ()
chat use var = do
    let hand = han use
    end <- hIsEOF hand
    if end
        then do
            putStrLn "Closing client"
            userQuit use var
            --no goodbye because client has closed connection
            hClose hand
        else do
            recv <- hGetLine hand
            if (recv == ":q")
                then do
                    putStrLn "Closing client"
                    userQuit use var
                    hPutStrLn hand "Goodbye!"
                    hClose hand
                else do
                    userList <- takeMVar var
                    broadcastMessage use ((show use) ++ ": " ++ recv) userList
                    putMVar var userList
                    chat use var

introduce :: Handle -> IO ()
introduce hand = hPutStrLn hand "Welcome to the chat Server!"
    >> hPutStrLn hand "Enter \":q\" or close telnet to quit"

quitCheck :: String -> Bool
quitCheck str
    | length str < 2 = False
    | head str == ':' && (str !! 1) == 'q' = True
    | otherwise = False
