module Locking ( FileState , available , readOnly , unavailable ) where

data FileState = Available
               | ReadOnly
               | Unavailable
               deriving Show

available   = Available
readOnly    = ReadOnly
unavailable = Unavailable