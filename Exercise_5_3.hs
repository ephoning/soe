myLength1 l = let myInc a b = a+1 in
                foldl myInc 0 l

myLength2 l = foldl (\ b a -> b+1) 0 l


 
