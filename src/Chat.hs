{- Alec Snyder
- chatServer Library
- includes socket functions
-}

module Chat (runServer) where

import Control.Exception
import Control.Concurrent
import Network
import System.Environment
import System.IO


data User = User {ident :: Int
                 ,han :: Handle}
    deriving(Eq)

instance Show User where
    show u = show (ident u)

--writeMessage: writes a string to a given user
writeMessage :: String -> User -> IO ()
writeMessage str u = do
    let hand = han u
    hPutStrLn (hand) str

--joinUser: given a handle, it assigns a username and adds the user
--to the list
--returns the new user
--I use the evaluate function to enforce strictness in MVars
joinUser :: Handle -> MVar [User] -> IO User
joinUser fileHandle var = do
    ls <- takeMVar var
    let biggestUser = grabUserName ls
    u <- evaluate $ User (biggestUser + 1) fileHandle
    let newList = (u:ls)
    mapM_ (writeMessage ((show u) ++ " has joined")) newList
    putMVar var newList
    return u where
        grabUserName [] = 0
        grabUserName (x:_) = ident x

--broadcastMessage: broadcasts a message from user u to every user but u
--use MVar to lock writes to handles to prevent multiple threads
--from writing to the same handle
broadcastMessage :: User -> String -> [User] -> IO ()
broadcastMessage _ _ [] = return () --shouldn't happen
broadcastMessage u str (x:xs)
    | x == u = mapM_ (writeMessage str) xs
    | otherwise = do
        writeMessage str x
        broadcastMessage u str xs

--removes the user from the user list and broadcasts to
--everyone else that the user has left
userQuit :: User -> MVar [User] -> IO ()
userQuit u var = do
    ls <- takeMVar var
    broadcastMessage u ((show u) ++ " has left") ls
    newList <- evaluate $ remUser u ls
    putMVar var newList where
        remUser _ [] = [] --shouldn't happen
        remUser user (x:xs)
            | user == x = xs
            | otherwise = x : remUser user xs

------------------------------------------------------------------------
{- All the socket function and socket handlers -}


--runServer: Set up sockets and start the server
runServer :: IO ()
runServer = withSocketsDo $ do
    portStr <- getEnv "CHAT_SERVER_PORT"
    sock <- listenOn $ Service portStr
    putStrLn $ "Listening on port " ++ portStr
    var <- newMVar []
    acceptCons sock var

--acceptCons: loops and accepts connections and then spawns child procs
--to handle clients
acceptCons :: Socket -> MVar [User] -> IO ()
acceptCons sock var = do
    (fileHandle,_, _) <- accept sock
    _ <- forkFinally 
        (handleClient fileHandle var) 
        (\_ -> hClose fileHandle)
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


--chat: Takes in a user and user list and loops over input
--on messages received it broadcasts to all users but the selected one
--on EOF or ":q" it closes handles and exits the thread
chat :: User -> MVar [User] -> IO ()
chat use var = do
    let hand = han use
    end <- hIsEOF hand
    closedHandle <- hIsClosed hand
    if (end || closedHandle)
        then do
            userQuit use var
            --no goodbye because client has closed connection
        else do
            recv <- hGetLine hand
            if (recv == ":q")
                then do
                    userQuit use var
                    hPutStrLn hand "Goodbye!"
                else do
                    userList <- takeMVar var
                    broadcastMessage 
                        use 
                        ((show use) ++ ": " ++ recv) 
                        userList
                    putMVar var userList
                    chat use var

--introduce: send a welcome message
introduce :: Handle -> IO ()
introduce hand = do
    hPutStrLn hand "Welcome to the chat Server!"
    hPutStrLn hand "Enter \":q\" or close telnet to quit"
