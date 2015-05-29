{- Alec Snyder
- lab 2 chat server program
- runner of runServer from the Chat library
- github link: https://github.com/allonsy/chatServer
-}
module Main (main) where

import Chat

{-Main: call runServer
the runServer function is in the Chat module so that it can be exposed
as a library function to the test suite which allows it to be called and
thoroughly tested from hspec. If all the socket stuff is put in here,
the test suite would not be able to call it since the test-suite is
already in a Main module and can't import another Main. Additionally,
cabal can't handle more than 1 library so we can't make this a non-Main
module as a workaround.
-}

main :: IO ()
main = runServer
