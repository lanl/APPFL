{-# LANGUAGE NoImplicitPrelude #-}

module Mod1 where

data D = A | B
data Bool = True | False

isA :: D -> Bool
isA A = True
isA _ = False
