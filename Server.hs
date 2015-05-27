{- Alec Snyder
- lab 2 chat server program
- following networking from O'sullivan's Real World Haskell
-}
module Main (main) where

import Network.Socket
import Network.BSD
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO
import System.Environment

import Chat

--Main: Set up socket and start forking
main :: IO ()
main = do
    portStr <- getEnv "CHAT_SERVER_PORT"
    let port = read portStr :: Int
    --now we grab our address information
    putStrLn "Grabbing server information"
    addrinfo <- getAddrInfo 
                (Just (defaultHints {addrFlags = [AI_PASSIVE]})) 
                Nothing 
                (Just portStr)
    let address = head addrinfo
    
    --set up TCP (Stream) socket
    putStrLn "creating socket"
    sock <- socket (addrFamily address) Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    
    --attempt to bind
    putStrLn "binding to socket"
    bindSocket sock (addrAddress address)
    
    --listen on socket
    listen sock 10
    
    var <- newMVar []
    
    --call function to accept connections
    acceptCons sock var

--acceptCons: loops and accepts connections and then spawns child procs
--to handle clients

acceptCons :: Socket -> MVar [User] -> IO ()
acceptCons sock var = do
    (conn, client) <- accept sock
    putStrLn $ "Client connected from ip: " ++ (show client) 
    forkIO (handleClient conn var)
    acceptCons sock var

handleClient :: Socket -> MVar [User] -> IO ()
handleClient sock var= do
    hand <- socketToHandle sock ReadWriteMode
    hSetBuffering hand LineBuffering
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
            hClose hand
        else do
            recv <- hGetLine hand
            --putStrLn $ "received: " ++ recv
            userList <- takeMVar var --lock to avoid concurrent writes
            broadcastMessage use ((show use) ++ ": " ++ recv) userList
            putMVar var userList
            chat use var
