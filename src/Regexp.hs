module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Show, Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative a reg = case reg of
                    Empty -> Empty
                    Epsilon -> Empty
                    Char b -> if (a == b) then Epsilon else Empty
                    Seq reg1 reg2 -> if (nullable reg1) then (Alt (Seq (derivative a reg1) reg2) (derivative a reg2)) else (Seq (derivative a reg1) reg2)  
                    Alt reg1 reg2 -> Alt (derivative a reg1) (derivative a reg2)
                    Star r -> Seq (derivative a r) reg

nullable :: Regexp -> Bool
nullable reg = case reg of
          Empty -> False
          Epsilon -> True
          Char b -> False
          Seq reg1 reg2 -> (nullable reg1) && (nullable reg2)
          Alt reg1 reg2 -> (nullable reg1) || (nullable reg2)
          Star r -> True
