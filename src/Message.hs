module Message (Message, readF, writeF, deleteF, existsF, stateF, hello, finish) where

import Text.Read

data Message = Read   FilePath
             | Write  FilePath
             | Delete FilePath
             | Exists FilePath
             | State  FilePath
             | Hello
             | Finish
             deriving (Show, Read)


readF, writeF, deleteF, existsF, stateF :: FilePath -> Message
readF   = Read
writeF  = Write
deleteF = Delete
existsF = Exists
stateF  = State

hello, finish :: Message
hello  = Hello
finish = Finish