import Data.Char
import Data.List
import System.Environment
import System.IO  
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.List (intersperse)
import Data.Maybe
import System.IO.Unsafe
import Tokens
import Grammar


--most functions have a selfdescribing name so i ommited commenting them in detail
		
main = do {
{-
     
    -- putStrLn $ (show args) 
	 
	 ; print contents
	
	 ; let files = ["A","B","C"]
	 ; let readorder = ["x1","z","x2","z","x1","x3","x4"]
	 ; let printorder = ["x3","x1","x2","x4"]
	 ; let ex = ["z"]
	 ; let eg = [("x2","x4")] 
	 
	 -}
	 ; args <- getArgs
	 ; handle <- openFile (head args) ReadMode
	 ; contents <- hGetContents handle
	 --; print contents
	 ; let src = (gram.alexScanTokens) contents
	 ; let files = (getFileNamesFromTokens src)
	 ; let readorder = (getReadOrderFromTokens src)
	 ; let printorder = (getPrintOrderFromTokens src)
	 ; let ex = (getExclusionsFromTokens src)
	 ; let eg = (getEqualitiesFromTokens src)
	 
	 
	 
	 --please ignore these comments, used for testing
	 {-
	 ; print (getFirstWord contents)
	 ; print (removeFirstWord contents)
	 ; print (isLast contents)
	 ; print (isLast (removeFirstWord(removeFirstWord contents)))
	 ; print (getSecondFromEq (getAfterNewLine(getAfterNewLine contents))) 
	 ; print (files!!1)
	 ; print (readCsv "A.csv")
	 ; print (readAllFiles (addDotCSV files))
	 ; print (prodCarthesianListList [["a","b"],["c","d"],["e","f"]] [["1","2"],["3","4"],["5","6"]]) 
	-- ; print (prodCarthesian (readAllFiles (addDotCSV files)))
	 ; let readorderafterex = ["x1","x2","x1","x3","x4"]
	 ; print (readFiles files)
	 ; print (flatten(getPositionOfStrings ex readorder))
	 ; print (removeExistAux (sort(flatten(getPositionOfStrings ex readorder))) (readFiles files))
	 ; print (removeExist ex readorder (readFiles files)) 
	 ; print (removeExistFromReadOrder ex readorder)
	 ; print (getDuplicates readorder)
	 ; print (removeInvalidLinesDuplicate readorderafterex (removeExist ex readorder (readFiles files)))
	 ; print (removeDuplicatesFromList readorderafterex)
	 ; print (variableRepetition readorderafterex (removeInvalidLinesDuplicate readorderafterex (removeExist ex readorder (readFiles files))))
	 ; print (solveVariableRepetition readorderafterex (removeExist ex readorder (readFiles files)))
	 ; print (solveEg eg (removeDuplicatesFromList readorderafterex) (solveVariableRepetition readorderafterex (removeExist ex readorder (readFiles files))))
	 ; print (solvePrintOrder printorder (removeDuplicatesFromList readorderafterex) (solveEg eg (removeDuplicatesFromList readorderafterex) (solveVariableRepetition readorderafterex (removeExist ex readorder (readFiles files)))))
    
	 
	 ; let csv = readFiles files
	 ; let csvAfterEx = removeExist ex readorder csv
	 ; let readorderAfterEx = removeExistFromReadOrder ex readorder
	 ; let csvAfterDuplicates = solveVariableRepetition readorderAfterEx (removeInvalidLinesDuplicate readorderAfterEx csvAfterEx)
	 ; let readorderAfterDuplicates = removeDuplicatesFromList readorderAfterEx
	 ; let csvAfterEg = solveEg eg readorderAfterDuplicates csvAfterDuplicates
	 ; let csvAfterOrder = sort (solvePrintOrder printorder readorderAfterDuplicates csvAfterEg)
	 ; print csvAfterOrder
	 
	 
	 ; print "asdf"
	 
	 ; let csv = readFiles files
     ; print "asdf"
	 ; let csvAfterDuplicates = solveVariableRepetition readorder (removeInvalidLinesDuplicate readorder csv)
	 ; print "asdf"
	 ; let readorderAfterDuplicates = removeDuplicatesFromList readorder
	 ; print "asdf"
	 ; let csvAfterEx = removeExist ex readorderAfterDuplicates csvAfterDuplicates
	 ; print "asdf"
	 ; let readorderAfterEx = removeExistFromReadOrder ex readorderAfterDuplicates
	 ; print "asdf"
	 ; let csvAfterEg = solveEg eg readorderAfterEx csvAfterEx
	 ; print "asdf"
	 ; let csvAfterOrder = sort (solvePrintOrder printorder readorderAfterEx csvAfterEg)
	 ; print "asdf1"
	 ; print csvAfterOrder
	
	 
	 ; print src 
	 ; print files
	 ; print readorder 
	 ; print printorder 
	 ; print ex 
	 ; print eg 
	  -}
	  
	; let csv = readFiles files
	; let csvAfterEg = solveEg eg readorder csv
		 
	; let csvAfterDuplicates = solveVariableRepetition readorder (removeInvalidLinesDuplicate readorder csvAfterEg)
	; let readorderAfterDuplicates = removeDuplicatesFromList readorder
		 
	; let csvAfterEx = removeExist ex readorderAfterDuplicates csvAfterDuplicates
	; let readorderAfterEx = removeExistFromReadOrder ex readorderAfterDuplicates

		 
	; let csvAfterOrder = sort (solvePrintOrder printorder readorderAfterEx csvAfterEx)
		 
	--; print csvAfterOrder
	
	; putStrLn (printer csvAfterOrder)
	
 }

-- ["x3","x1","x2","x4"] 0 ["x1","x2","x3","x4"] ["1","2","3","4"]

--print function
printer :: [[String]] -> String
printer [] = [] 
printer (x:xs) = printerAux x ++ "\n" ++ printer xs

printerAux :: [String] -> String
printerAux xs = intercalate "," xs


--not used
areAnyFilesEmpty :: [String] -> Bool
areAnyFilesEmpty (x:xs) | isFileEmpty x = True 
 | otherwise = False || areAnyFilesEmpty xs
areAnyFilesEmpty [] = False 

isFileEmpty :: String -> Bool
isFileEmpty xs | readCsv (addDotCSV xs) == [] = True
 | otherwise = False
 where addDotCSV xs = xs ++ ".csv"
--end not used

--get stuff from the tokens
getFileNamesFromTokens :: FileName -> [String]
getFileNamesFromTokens (Filename x _ fnam) = x : getFileNamesFromTokens fnam
getFileNamesFromTokens (Filenam x _ _) = [x]
getFileNamesFromTokens (Filename1 x _ _ _) = [x]
getFileNamesFromTokens (Filename2 x _ _ _) = [x]
getFileNamesFromTokens (Filename3 x _ _ _ _) = [x]
getFileNamesFromTokens (Filename4 x _ _ _ _) = [x]




getReadOrderFromTokens :: FileName -> [String]
getReadOrderFromTokens (Filename _ xs fnam) = (getReadOrderFromFile xs) ++ getReadOrderFromTokens fnam
getReadOrderFromTokens (Filenam _ xs _) = getReadOrderFromFile xs
getReadOrderFromTokens (Filename1 _ xs _ _) = getReadOrderFromFile xs
getReadOrderFromTokens (Filename2 _ xs _ _) = getReadOrderFromFile xs
getReadOrderFromTokens (Filename3 _ xs _ _ _) = getReadOrderFromFile xs
getReadOrderFromTokens (Filename4 _ xs _ _ _) = getReadOrderFromFile xs

getReadOrderFromFile :: ReadOrder -> [String]
getReadOrderFromFile (OrdName x r) = x : getReadOrderFromFile r
getReadOrderFromFile (OrdNam x) = [x]



getPrintOrderFromTokens :: FileName -> [String]
getPrintOrderFromTokens (Filename _ xs fnam) = getPrintOrderFromTokens fnam
getPrintOrderFromTokens (Filenam _ _ xs) = getPrintOrderFromPrintOrder xs
getPrintOrderFromTokens (Filename1 _ _ _ xs) = getPrintOrderFromPrintOrder xs
getPrintOrderFromTokens (Filename2 _ _ _ xs) = getPrintOrderFromPrintOrder xs
getPrintOrderFromTokens (Filename3 _ _ _ _ xs) = getPrintOrderFromPrintOrder xs
getPrintOrderFromTokens (Filename4 _ _ _ _ xs) = getPrintOrderFromPrintOrder xs

getPrintOrderFromPrintOrder :: PrintOrder -> [String]
getPrintOrderFromPrintOrder (PrintName x p) = x : getPrintOrderFromPrintOrder p
getPrintOrderFromPrintOrder (PrintNam x) = [x]



getExclusionsFromTokens :: FileName -> [String]
getExclusionsFromTokens (Filename _ _ fnam) = getExclusionsFromTokens fnam
getExclusionsFromTokens (Filenam _ _ _) = []
getExclusionsFromTokens (Filename1 _ _ xs _) = getExclusions xs
getExclusionsFromTokens (Filename2 _ _ _ _) = []
getExclusionsFromTokens (Filename3 _ _ xs _ _) = getExclusions xs
getExclusionsFromTokens (Filename4 _ _ _ xs _) = getExclusions xs

getExclusions :: ExOrder -> [String]
getExclusions (ExName x ex) = x : getExclusions ex
getExclusions (ExNam x) = [x]




getEqualitiesFromTokens :: FileName -> [(String,String)]
getEqualitiesFromTokens (Filename _ _ fnam) = getEqualitiesFromTokens fnam
getEqualitiesFromTokens (Filenam _ _ _) = []
getEqualitiesFromTokens (Filename1 _ _ _ _) = []
getEqualitiesFromTokens (Filename2 _ _ xs _) = getEqualities xs
getEqualitiesFromTokens (Filename3 _ _ _ xs _) = getEqualities xs
getEqualitiesFromTokens (Filename4 _ _ xs _ _) = getEqualities xs

getEqualities :: EgOrder -> [(String,String)]
getEqualities (EgName x y eq) = (x,y) : getEqualities eq
getEqualities (EgNam x y) = [(x,y)]




--interpreter

solvePrintOrder :: [String] -> [String] -> [[String]] -> [[String]]
solvePrintOrder printorder readorder (x:xs) = (solvePrintOrderAux printorder 0 readorder x) : solvePrintOrder printorder readorder xs
solvePrintOrder _ _ [] = []

solvePrintOrderAux :: [String] -> Int -> [String] -> [String] -> [String]
solvePrintOrderAux (x:printorder) i readorder xs | index /= i = solvePrintOrderAux printorder (i+1) (swapElementsAt index i readorder) (swapElementsAt i index xs)
 | otherwise = solvePrintOrderAux printorder (i+1) readorder xs
 where index = head (getPosOfStringInList x readorder)
solvePrintOrderAux [] _ _ xs = xs


swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs | i < j = swapElementsAtAux i j xs
 | otherwise = swapElementsAtAux j i xs

swapElementsAtAux :: Int -> Int -> [a] -> [a]
swapElementsAtAux i j xs = let elemI = xs !! i
                               elemJ = xs !! j
                               left = take i xs
                               middle = take (j - i - 1) (drop (i + 1) xs)
                               right = drop (j + 1) xs
                           in  left ++ [elemJ] ++ middle ++ [elemI] ++ right 
 

 
-- take list of eq pairs | readorder | csv returns solved csv 
solveEg :: [(String,String)] -> [String] -> [[String]] -> [[String]]
solveEg xs order (y:ys) | solveEgAux2 xs order y = y : solveEg xs order ys
 | otherwise = solveEg xs order ys
solveEg _ _ [] = [] 

solveEgAux2 :: [(String,String)] -> [String] -> [String] -> Bool
solveEgAux2 (x:xs) order ys | solveEgAux x order ys = True && (solveEgAux2 xs order ys)
 | otherwise = False 
solveEgAux2 [] _ _ = True
 
solveEgAux ::  (String,String) -> [String] -> [String] -> Bool
solveEgAux (x,y) order xs | xs !! (head (getPosOfStringInList x order)) == xs !! (head(getPosOfStringInList y order)) = True
 | otherwise = False
 
solveVariableRepetition :: [String] -> [[String]] -> [[String]]  
solveVariableRepetition readorderafterex ys = (variableRepetition readorderafterex (removeInvalidLinesDuplicate readorderafterex ys))
 
 
 
--return readorder after exist eliminated
removeExistFromReadOrder :: [String] -> [String] -> [String] 
removeExistFromReadOrder (x:xs) ys = removeExistFromReadOrder xs (removeAllAppareanceOfString x ys)
removeExistFromReadOrder [] ys = ys

removeAllAppareanceOfString :: String -> [String] -> [String]
removeAllAppareanceOfString x (y:xs) | x==y && length xs /=0 = removeAllAppareanceOfString x xs
 | x==y && length xs ==0 = []
 | otherwise = y : (removeAllAppareanceOfString x xs) 
removeAllAppareanceOfString _ [] = []
 
 
 
--returns [[]] that needs to be printed, unordered
removeExist :: [String] -> [String] -> [[String]] -> [[String]]
removeExist ex readorder ys =  removeExistAux (sort (flatten (getPositionOfStrings ex readorder))) ys
removeExist [] _ ys = ys
 
removeExistAux :: [Int] -> [[String]] -> [[String]]
removeExistAux (x:xs) ys = removeExistAux (minusOne xs) (removeCol x ys)
removeExistAux [] ys = ys

minusOne :: [Int] -> [Int]
minusOne xs = map aux xs
 where aux x = x-1

flatten :: [[a]] -> [a]
flatten (x:xs) = x ++ flatten xs
flatten [] = []




 
--return true if string is present in list
contains :: String -> [String] -> Bool
contains x (y:xs) | x==y = True
 | otherwise = contains x xs || False
contains _ [] = False




--return pos of string in list
getPosOfStringInList :: String -> [String] -> [Int]
getPosOfStringInList x xs = getPosOfStringInListAux x xs 0

getPosOfStringInListAux :: String -> [String] -> Int -> [Int]
getPosOfStringInListAux x (y:xs) i | x == y = i : getPosOfStringInListAux x xs (i+1)
 | otherwise  = getPosOfStringInListAux x xs (i+1)
getPosOfStringInListAux _ [] _ = []



--return pos of all strings in a list
getPositionOfStrings :: [String] -> [String] -> [[Int]]
getPositionOfStrings (x:xs) ys = getPosOfStringInList x ys : getPositionOfStrings xs ys
getPositionOfStrings [] _ = []



-- string will be found in list. 100%
findPositionOfString :: String -> [String] -> Int 
findPositionOfString x (y:xs) | x==y = 0
 | otherwise = 1 + findPositionOfString x xs


 
 
removeDuplicatesFromList :: [String] -> [String]
removeDuplicatesFromList xs | dup == "" = xs
 | otherwise = removeDuplicatesFromList (removeStringFromList dup xs)
 where dup = findFirstDuplicate xs
  
--this should never be called with a string that is not in the string list
--removes first appearence of string
removeStringFromList :: String -> [String] -> [String]
removeStringFromList x (y:xs) | x==y = xs
 | otherwise = y : (removeStringFromList x xs) 

findFirstDuplicate  :: [String] -> String
findFirstDuplicate (x:xs) | isStringFoundInList x xs = x
 | otherwise = findFirstDuplicate xs
findFirstDuplicate [] = ""

isStringFoundInList :: String -> [String] -> Bool
isStringFoundInList x (y:xs) | x==y = True
 | otherwise = isStringFoundInList x xs
isStringFoundInList _ [] = False 




getDuplicates :: [String] -> [String] 
getDuplicates xs = removeDuplicatesFromList (getDuplicatesAux xs)

getDuplicatesAux :: [String] -> [String]
getDuplicatesAux (x:xs) | contains x xs = x : getDuplicatesAux xs
 | otherwise = getDuplicatesAux xs
getDuplicatesAux [] = [] 








-- readorder,csv -> solved csv
removeInvalidLinesDuplicate :: [String] -> [[String]] -> [[String]]
removeInvalidLinesDuplicate xs ys = removeInvalidLinesDuplicateAux (getPositionOfStrings(getDuplicates xs) xs) ys
removeInvalidLinesDuplicate [] ys = ys
--[[int]] pos of duplicates
removeInvalidLinesDuplicateAux :: [[Int]] -> [[String]] -> [[String]]
removeInvalidLinesDuplicateAux xs (y:ys) | isValid xs y = y : removeInvalidLinesDuplicateAux xs ys
 | otherwise = removeInvalidLinesDuplicateAux xs ys
removeInvalidLinesDuplicateAux xs [] = []


isValid :: [[Int]] -> [String] -> Bool
isValid (x:xs) y | isValidAux x y = True && isValid xs y
 | otherwise = False 
isValid [] _ = True
 
isValidAux :: [Int] -> [String] -> Bool
isValidAux (x:xs) ys | length xs == 0 = True
 | ys!!x == ys!!(head xs) = True && isValidAux xs ys
 | otherwise = False 




variableRepetition :: [String] -> [[String]] -> [[String]] 
variableRepetition xs ys = variableRepetitionAux xs ys 0

variableRepetitionAux :: [String] -> [[String]] -> Int -> [[String]] 
variableRepetitionAux (x:xs) ys i | contains x xs = variableRepetitionAux xs (removeCol i ys) i
 | otherwise = variableRepetitionAux xs ys (i+1)
variableRepetitionAux [] ys i = ys								 






removeNthElement :: Int -> [a] -> [a]
removeNthElement _ []     = []
removeNthElement i (a:xs) | i == 0    = xs
 | otherwise = a : removeNthElement (i-1) xs
 
 
 
 
removeCol :: Int ->  [[a]] -> [[a]]
removeCol x xs = map (removeNthElement x) xs



--used if there are no empty files that should have stuff that needs printed
readFiles :: [String] -> [[String]]
readFiles x | length x == 1 = head (readAllFiles (addDotCSV x)) 
 | otherwise = prodCarthesian (readAllFiles (addDotCSV x))
 where addDotCSV x = map ( ++ ".csv") x

 
prodCarthesian :: [[[a]]] ->[[a]]
prodCarthesian (x:xs) | length xs == 0 = x
 | otherwise = prodCarthesian ((prodCarthesianListList x (xs !! 0)) : (drop 1 xs))

prodCarthesianListList :: [[a]] -> [[a]] -> [[a]]
prodCarthesianListList (x:xs) ys =  prodCarthesianLineList x ys ++ prodCarthesianListList xs ys
prodCarthesianListList [] _ = [] 

prodCarthesianLineList :: [a] -> [[a]] -> [[a]]
prodCarthesianLineList xs ys = map (xs ++) ys

readAllFiles :: [String] -> [[[String]]]
readAllFiles (x:xs) = readCsv x: readAllFiles xs
--readAllFiles (x:xs) = (unsafePerformIO (readCsv x )): readAllFiles xs
readAllFiles [] = []
 
 

--splits a string into an array of strings
split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s


--cvs reader
--i know unsafePerformIO is unsafe, i had no ideea how else to convert from type IO ab to type ab
readCsv :: String -> [[String]]
readCsv xs = readCsv3 (unsafePerformIO(readCsv1 xs))

readCsv3 (x:xs) = (split ',' x) : readCsv3 xs
readCsv3 [] = []

readCsv1 xs = do
            c <- readCsv2 xs 
            let lists = lines c
            return lists

readCsv2 xs = do
              handle <- openFile (xs) ReadMode
              c <- hGetContents handle 
              return c
			  
			  
