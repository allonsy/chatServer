# chatServer By Alec Snyder
* Basic Chat Server implemented in haskell
* chat room like functionality
* github link: github link: https://github.com/allonsy/chatServer
* email: alsnyder@uchicago.edu or linuxbash8@gmail.com

##Build instructions:
* Clone the repository or untar the cabal sdist tar.gz file
* You will need the Network and Hspec packages so make sure that you build
from a cabal sandbox with these packages installed or have these 
packages globally installed.

run 
* `cabal configure`
* `cabal build`

to run the tests, run:
* `cabal configure --enable-tests`
* `cabal test`


##Executing instructions
* set the CHAT_SERVER_PORT environment variable
* to run it execute `dist/build/chat/chat`
* or you may execute `cabal run`
* You may use telnet as a client, go ahead and connect to the ip of
the server at the port pointed to by the environment variable
* I have provided in the github repo a quick bash script that
calls telnet with the desired parameters. Its syntax is: 
 `./chat_client 128.135.221.121` or `./chat_client localhost`

## Using telnet as a client
* just connect via telnet as mentioned above
* type messages as normal and hit enter to send them to the server
* quit telnet or type ":q" to quit
