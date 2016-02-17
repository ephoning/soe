 {-# LANGUAGE UnicodeSyntax #-}

module Exercise_3_1 where

myPutStr ∷ String → IO ()
myPutStr [] = return ()
myPutStr (c : s) = do
  putChar c
  myPutStr s

myGetLine ∷ IO [Char]
myGetLine = do
  c ← getChar
  if c ==  '\n'
    then return ""
    else do
      s ← myGetLine
      return (c : s)
