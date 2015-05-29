{- Alec Snyder
- Test Suite for chatServer
- github link: https://github.com/allonsy/chatServer
-}

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Network
import System.Environment
import System.IO
import Test.Hspec


import Chat

chatPort :: String
chatPort = "6667"

--connectClient: Connects one client to the server and returns the 
--resulting file handle
connectClient :: IO Handle
connectClient = do
    hand <- connectTo "localhost" (Service chatPort)
    hSetNewlineMode hand (NewlineMode CRLF CRLF)
    hSetBuffering hand LineBuffering
    return hand

--connectNClients: allows you to connect multiple clients 
--in rapid succession
connectNClients :: Int -> IO [Handle]
connectNClients n = replicateM n connectClient

--connects a client and ensures that they see a welcome message
checkWelcomeMessage :: IO Bool
checkWelcomeMessage = do
    han1 <- connectClient
    str1 <- hGetLine han1
    str2 <- hGetLine han1
    ret <- evaluate $ (str1 == "Welcome to the chat Server!") 
        && (str2 == "Enter \":q\" or close telnet to quit")
    closeHand han1
    return ret

--helper function to gracefully close a handle
closeHand :: Handle -> IO ()
closeHand hand = do
    hPutStrLn hand ":q"

--joins three users and then ensures that each sees a 
-- "N has joined" message with the proper N values
checkIncrementId :: IO Bool
checkIncrementId = do
    ls <- connectNClients 3
    idList <- mapM getUserName ls
    ret <- evaluate $ (idList == ['1','2','3'])
    closeHand $ ls !! 0
    closeHand $ ls !! 1
    closeHand $ ls !! 2
    return ret

--given a new connection, gets the user name 
--(doesn't work with more than 9 clients)
getUserName :: Handle -> IO Char
getUserName hand = do
    _ <- hGetLine hand
    _ <- hGetLine hand
    str3 <- hGetLine hand
    return $ head str3

--ensures that users already in the channel see the proper
--"N has joined" message when a user joins
joinMessageCheck :: IO Bool
joinMessageCheck = do
    user1 <- connectClient
    user2 <- connectClient
    _ <- getUserName user1
    name2 <-getUserName user2
    recv <- hGetLine user1
    ret <- evaluate $ recv == (name2 : " has joined")
    closeHand user1
    closeHand user2
    return ret

--checks that graceful quits work and display goodbye message
closeGracefulCheck :: IO Bool
closeGracefulCheck = do
    user <- connectClient
    _ <- getUserName user
    hPutStrLn user ":q"
    end <- hGetLine user
    return $ end == "Goodbye!"

--makes sure that messages sent arrive to all users (that aren't the
--sender)
messageReceiptCheck :: IO Bool
messageReceiptCheck = do
    user1 <- connectClient
    user2 <- connectClient
    user3 <- connectClient
    _ <- getUserName user1
    _ <- getUserName user2
    _ <- getUserName user3
    hPutStrLn user1 "Hello There!"
    _ <- hGetLine user2
    message <- hGetLine user2
    message2 <- hGetLine user3
    ret <- evaluate $ (message == message2) 
        && (message == "1: Hello There!")
    closeHand user1
    closeHand user2
    closeHand user3
    return ret

--echoCheck: makes sure messages sent by user aren't echoed to that user
echoCheck :: IO Bool
echoCheck = do
    user1 <- connectClient
    user2 <- connectClient
    _ <- hGetLine user1
    _ <- getUserName user1
    _ <- getUserName user2
    hPutStrLn user1 "Hello There!"
    ret <- hReady user1
    closeHand user1
    closeHand user2
    return $ not ret

--checks that when a user leaves, all other users see that
--the user has left with the proper message
leftMessageCheck :: IO Bool
leftMessageCheck = do
    user1 <- connectClient
    user2 <- connectClient
    user3 <- connectClient
    _ <- getUserName user1
    _ <- getUserName user2
    _ <- getUserName user3
    closeHand user1
    _ <- hGetLine user2
    message <- hGetLine user2
    message2 <- hGetLine user3
    ret <- evaluate $ (message == message2) 
        && (message == "1 has left")
    closeHand user2
    closeHand user3
    return ret
    
main :: IO ()
main = do
    setEnv "CHAT_SERVER_PORT" chatPort
    _ <- forkIO runServer
    threadDelay 1000000 --allow for server to set up
    
    hspec $ describe "Testing Chat Server" $ do
        describe "Connection successful/introduction message" $ do
            it "connects and succesfully sends welcome message" $
                checkWelcomeMessage `shouldReturn` True
            it "Should increment username IDs" $
                checkIncrementId `shouldReturn` True
        
        describe "joins users" $ do
            it "shows join message with appropriate userID" $
                joinMessageCheck `shouldReturn` True
        
        describe "closes gracefully" $ do
            it "works with exit command and displays goodbye message" $
                closeGracefulCheck `shouldReturn` True
        
        describe "sending messages" $ do
            it "succesfully sends messages to other users" $
                messageReceiptCheck `shouldReturn` True
            it "doesn't echo to current user" $
                echoCheck `shouldReturn` True
        describe "Leaving channels" $ do
            it "succesfully sends leave messages to other users" $
                leftMessageCheck `shouldReturn` True
