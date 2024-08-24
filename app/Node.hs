module Node where

data Node = Node
    {
        id :: Int,
        prev :: Int,
        next :: Int
    }