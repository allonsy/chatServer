{- Alec Snyder
- chatServer Library
-}

module Chat (User(User),han,joinUser,broadcastMessage,userQuit) where

import System.IO
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base

data User = User {ident :: Int
                 ,han :: Handle}
    deriving(Eq)

instance Show User where
    show u = show (ident u)

writeMessage :: String -> User -> IO ()
writeMessage str u = hPutStrLn (han u) str

joinUser :: Handle -> MVar [User] -> IO User
joinUser han var = do
    ls <- takeMVar var
    let biggestUser = grabUserName ls
    u <- evaluate $ User (biggestUser + 1) han
    newList <- evaluate (u:ls)
    mapM_ (writeMessage ((show u) ++ " has joined")) newList
    putMVar var newList
    return u where
        grabUserName [] = 0
        grabUserName (x:xs) = ident x

--broadcastMessage: broadcasts a message from user u to every user but u
broadcastMessage :: User -> String -> [User] -> IO ()
broadcastMessage _ _ [] = return ()
broadcastMessage u str (x:xs)
    | x == u = mapM_ (writeMessage (str)) xs
    | otherwise = do
        writeMessage ((show u) ++ ": " ++ str) x
        broadcastMessage u str xs

userQuit :: User -> MVar [User] -> IO ()
userQuit u var = do
    ls <- takeMVar var
    broadcastMessage u ((show u) ++ " has left") ls
    newList <- evaluate $ remUser u ls
    putMVar var newList
    return () where
        remUser _ [] = []
        remUser user (x:xs)
            | user == x = xs
            | otherwise = x : remUser user xs
    
