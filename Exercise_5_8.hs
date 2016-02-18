{-# LANGUAGE UnicodeSyntax #-}

module Exercise_5_8 where

import Prelude.Unicode

--ignoring wrap-around @ 255 for now

encrypt ∷ String → String
encrypt "" = ""
encrypt (c:cs) = encrypt' c : encrypt cs where
  encrypt' c = toEnum ((fromEnum c) + 1)
  
decrypt ∷ String → String
decrypt "" = ""
decrypt (c:cs) = decrypt' c : decrypt cs where
  decrypt' c = toEnum ((fromEnum c) - 1)

  
