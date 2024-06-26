-- English:
-- Gödel numbering is a mapping that assigns a number to each expression, 
-- and is used to prove the famous Gödel's incompleteness theorem, 
-- but can also be thought of as a cipher. In this exercise, we will implement a simplified Gödel notation.

-- Hungarian:
-- A Gödel-számozás egy leképezés, amely minden kifejezéshez egy számot rendel hozzá, 
-- és a híres, Gödel-féle nemteljességi tétel bizonyításához használják, 
-- de felfogható kódolásként is. A feladatban egy egyszerűsített Gödel-számozást fogunk megvalósítani.

import Data.Maybe (fromMaybe)
import Data.List (group)

-- Creating the dictionary type
type Dictionary = [(Char, Int)]

-- This function creates a dictionary, it contains ordered pairs, 
-- first element is the character, 
-- the second element is the corresponding number/code
dictionary :: [Char] -> Dictionary
dictionary chars = zip chars [1..]
-- test: dictionary (['a'..'z'] ++ ['A'..'Z'])

--------------
-- ENCODING --
--------------
-- Finds the code assigned to the specified character
charToNum :: Dictionary -> Char -> Maybe Int
charToNum [] _ = Nothing
charToNum ((c,num):dict) char
    | c == char = Just num
    | otherwise = charToNum dict char
-- test: charToNum (dictionary ['a' .. 'z']) 'a'

-- Converts a text into its corresponding sequence of numbers,
-- uses the dictionary given as a parameter. 
-- If there are illegal (not existing in the dictionary) characters in the text, sets their value to 0.
translate :: Dictionary -> String -> [Int]
translate _ [] = []
translate dict (c:rest) =
    case charToNum dict c of
        Just num -> num : translate dict rest
        Nothing -> 0 : translate dict rest
-- test: translate (dictionary ['a' .. 'z']) "gödel" 

-- Generates infinite list of prime numbers using the Sieve of Eratosthenes
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
primeList :: [Integer]
primeList = sieve [2..]
    where sieve (el:rest) = el : sieve [x | x <- rest, x `mod` el /= 0]
-- test: take 10 primeList

-- Converts text to its corresponding Gödel number
-- formula: p1^n1 * p2^x2 * ... * pn^xn
-- where 
--       p1..pn are the primes in ascending order,
--       x1..xn are the numbers associated for the letters in the string
encode :: Dictionary -> String -> Integer
encode _ [] = 1  -- If the dictionary is empty, return 1
encode dict str = product $ zipWith (^) primeList (translate dict str)

-- In summary, this function takes a dictionary and a string, 
-- translates the string into a list of integers using the dictionary, 
-- raises each prime number to the power of the corresponding integer, 
-- and then calculates the product of all these numbers to get the Gödel number for the input string.

-- test: encode (dictionary ['a' .. 'z']) "abba"

--------------
-- DECODING --
--------------
-- Finds the character assigned to the specified number
numToChar :: Dictionary -> Int -> Maybe [Char]
numToChar [] _ = Nothing
numToChar ((c,num):dict) n
    | n == num = Just [c]
    | otherwise = numToChar dict n
-- test: numToChar (dictionary $ ['a' .. 'z'] ++ ['A' .. 'Z']) 42

-- Decomposes a positive integer into a product of prime factors
primeFactorization :: Integer -> [Integer]
primeFactorization n
    | n < 2 = []
    | otherwise = factorize n primeList
    where
        factorize :: Integer -> [Integer] -> [Integer]
        factorize m (p:ps)
            | m < p * p = [m]  -- if m is less than the square of the current prime then add it to the list
            | m `mod` p == 0 = p : factorize (m `div` p) (p:ps)  -- If m is divisible by p, add p to the factors list and continue with m/p
            | otherwise = factorize m ps  -- If m is not divisible by p, continue with the next prime in the list
-- In summary, this function takes a positive integer,
-- in case it is less than 2, it can't be decomposed to a product of primes, so we return an empty list,
-- otherwise we can factorize it, the factorize function takes the integer and the list of primes
-- if the integer is less than the square of the current prime then we return it as a list,
-- because m is only divisible with primes less then m's square root (it can't be further decomposed),
-- if that's not the case then if m is divisible by the current prime we add the prime to the list and
-- call the factorize function recursively with the divided integer and the same list of primes,
-- otherwise we call the factorize function with the integer (without dividing it by the prime) and
-- as the second argument the rest of the primes excluding the current prime.

-- test: primeFactorization 42

decode :: Dictionary -> Integer -> String
decode dict n = concatMap (fromMaybe "*" . numToChar dict) factorCounts -- if it fails to convert to the char, it returns *
    where
        primeFactors = primeFactorization n  -- Get prime factors of n
        factorCounts = map length $ group primeFactors  -- Count occurrences of each prime factor, "group" groups identical primes, like [2,3,3,5,5,7] -> [[2], [3,3], [5,5], [7]]
-- In summary, the decode function takes a dictionary and a Gödel number, 
-- calculates the prime factors and their number of occurences, 
-- converts each count to a character using the dictionary, 
-- and concatenates the characters to form the decoded string.

-- test: decode (dictionary ['a' .. 'z']) 1575

-- Function to encode text from an input file and write the encoded text to an encoded file
encodeTextFile :: Dictionary -> FilePath -> FilePath -> IO ()
encodeTextFile dict inputFile encodedFile = do
    -- Read text from input file
    inputText <- readFile inputFile
    -- Encode the input text
    let encodedText = encode dict inputText
    -- Write the encoded text to the encoded file
    writeFile encodedFile (show encodedText)

-- Function to decode text from an encoded file and write the decoded text to a decoded file
decodeTextFile :: Dictionary -> FilePath -> FilePath -> IO ()
decodeTextFile dict encodedFile decodedFile = do
    -- Read encoded text from encoded file
    encodedText <- readFile encodedFile
    -- Decode the encoded text
    let decodedText = decode dict (read encodedText :: Integer)
    -- Write the decoded text to the decoded file
    writeFile decodedFile decodedText

main :: IO ()
main = do
    let dict = dictionary (['a'..'z'] ++ ['\'', ' '])
    
    encodeTextFile dict "input.txt" "encoded.txt"
    decodeTextFile dict "encoded.txt" "decoded.txt"

--------------------------------------------
-- Proving Gödel's incompleteness theorem --
--------------------------------------------
-- Note: Gödel used complex mathematical concepts, arithmetic operations, logical symbols, 
-- we will prove the same thing he did, but in our own simplified system

dictionaryToProveTheorem :: Dictionary
dictionaryToProveTheorem = dictionary ['n', 'o', 'p', 'r', 'f', 's', 't', 'a', 'e', 'm', 'w', 'i', 'h', 'g', 'd', 'l',  'u', 'b']

proveGodel'sIncompletenessTheorem :: IO ()
proveGodel'sIncompletenessTheorem = do
    -- Define the statement G using only characters from the dictionary
    let statement = "no proof for statement with godel number 200110744078624342146261625871540938484073629315325247508879570347077401626924166475466579713308035014642704018470691449254401930458691164270882372195859047146351623455018370221871548270852737949425741177934664297734762538471101626737709188328267449001260512308827105716238428682761628937715526866238193384624720527723502262784518662338173673535696380517146278283120284670652702391702052974521677912774662236437307945710987997288453234355386969715025002033081514995470288178444889862"
    -- Encode the statement into its Gödel number
    let encodedStatement = encode dictionaryToProveTheorem statement
    
    -- Display the statement and its Gödel number
    putStrLn $ "\n" ++ "Statement: " ++ statement
    putStrLn $ "\n" ++ "Gödel number of the statement: " ++ show encodedStatement ++ "\n"
    -- Check if the Gödel number in the statement matches the actual Gödel number
    if encodedStatement == 200110744078624342146261625871540938484073629315325247508879570347077401626924166475466579713308035014642704018470691449254401930458691164270882372195859047146351623455018370221871548270852737949425741177934664297734762538471101626737709188328267449001260512308827105716238428682761628937715526866238193384624720527723502262784518662338173673535696380517146278283120284670652702391702052974521677912774662236437307945710987997288453234355386969715025002033081514995470288178444889862
        then putStrLn "The statement G matches its Gödel number, proving its own unprovability."
        else putStrLn "Error: Gödel number mismatch, the statement does not prove its own unprovability."
