import Data.List

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int
myAbs x | x >= 0 = x
        | otherwise = -x

myMin :: Int -> Int -> Int
myMin x y | x < y = x
        | otherwise = y

myMax :: Int -> Int -> Int
myMax x y | x > y = x
        | otherwise = y

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "List is empty"
myHead (a:_) = a

myTail :: [a] -> [a]
myTail [] = error "List is empty"
myTail (_:a) = a

myLength :: [a] -> Int
myLength [] = 0;
myLength (a:as) = (+1) (myLength as)

myNth :: [a] -> Int -> a
myNth a 0 = myHead a
myNth a b | b < 0 = error "negative index"
        | b >= myLength a = error "index out of bounds"
        | otherwise = myNth (myTail a) (b - 1)

myTake :: Int -> [a] -> [a]
myTake b a | myLength a == 0 = []
myTake 0 a = []
myTake b (a:as) = a : myTake (b - 1) as

myDrop :: Int -> [a] -> [a]
myDrop 0 a = a
myDrop b a | b >= myLength a = []
        | (0 < b && b < myLength a) = myDrop (b - 1) (myTail a)

myAppend :: [a] -> [a] -> [a]
myAppend [] b = b
myAppend (a:ax) b = a : myAppend ax b

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a:ax) = myAppend (myReverse ax) [a]

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit a =  myReverse( myTail (myReverse a))

myLast :: [a] -> a
myLast [] = error "empty list"
myLast a = myHead(myReverse a)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] bx = []
myZip ax [] = []
myZip (a:ax) (b:bx) = myTuple a b : myZip ax bx

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b):ax) = ((myAppend [a] (myFst ret)), (myAppend [b] (mySnd ret)))
        where ret = myUnzip ax

myMap :: (a -> b) -> [a] -> [b]
myMap a [] = []
myMap a (b:bx) = (a b) : myMap a bx

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter a (b:bx) | a b == True = b : myFilter a bx
                | a b == False = myFilter a bx
myFilter a [] = []

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl a b (c:cx) =  myFoldl a (a b c) cx
myFoldl a b [] = b

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr a b [] = b
myFoldr a b c = myFoldr a (a (myLast c) b) (myInit c)

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition a [] = ([], [])
myPartition a (b:bx) | a b == True = ((b : good), bad)
        | otherwise = (good, (b : bad))
        where (good, bad) = myPartition a bx

app :: [a] -> [a] -> [a]
app [] b = b
app (a:ax) b = a : app ax b

fs :: (a, b) -> a
fs (a, b) = a

sn :: (a, b) -> b
sn (a, b) = b

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort a [] = []
myQuickSort a (b:bx) = app(app(myQuickSort a (sn r)) [b])(myQuickSort a (fs r))
        where r = myPartition (a b) bx