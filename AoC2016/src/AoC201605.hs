{-# LANGUAGE FlexibleContexts #-}

module AoC201605
  ( runDay,
  ) where

import Data.ByteString.Lazy.Char8 (append, pack, unpack)
import Data.Char (digitToInt)
import Data.Digest.Pure.MD5
import Data.List (isPrefixOf, sortBy)
import qualified Data.Map.Lazy as Map

runDay :: IO ()
runDay = do
  putStrLn "Next answers take a while to calculate, so these answers are pre-caluclated."
  putStrLn "Uncomment in the source to force execution."
  putStrLn "5) The first password is 801b56a7."
  --putStrLn $ "5) The password is " ++ (take 8 $ findPassword realInput ) ++ "."
  putStrLn "5) The second password is 424a0197."
--  putStrLn $ "5) The second password is " ++ (findDoor2Password realInput) ++ "."

-- Part 2

findDoor2Password :: PasswordContainer -> [Char]
findDoor2Password pc = let
  startPw = [(0, '_'), (1, '_'), (2, '_'), (3, '_'), (4, '_'), (5, '_'), (6, '_'), (7, '_')]
  in fd2pw pc startPw

fd2pw :: PasswordContainer -> [(Int, Char)] -> [Char]
fd2pw pc pw
  | pComplete = extractPassword pw
  | isGood && pPosValid && pPosOpen = fd2pw nextPasswordContainer updatedPw 
  | otherwise = fd2pw nextPasswordContainer pw
  where
    digest = generateMD5 pc
    isGood = md5IsGoodForPassword $ show digest
    nextPasswordContainer = PasswordContainer (pcId pc) (1 + (counter pc))
    pPosValid = passwordPosValid digest
    pPos = passwordPos digest
    pChar = passwordChar 6 digest
    pPosOpen = (lookup pPos pw) == Just '_'
    updatedPw = updatePw pPos pChar pw
    pComplete = isPasswordComplete pw

extractPassword :: Ord a => [(a, b)] -> [b]
extractPassword pw = map (\x -> snd x) $ sortedPw pw

sortedPw :: Ord a => [(a, b)] -> [(a, b)]
sortedPw pw = sortBy (\x y -> compare (fst x) (fst y)) pw

updatePw :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
updatePw pos char xs = map (\x -> if (fst x) == pos then (pos, char) else x) xs

passwordPosValid :: MD5Digest -> Bool
passwordPosValid digest = (passwordPos digest) < 8

passwordPos :: MD5Digest -> Int
passwordPos digest = digitToInt $ head $ drop 5 $ show digest

isPasswordComplete :: Foldable t => t (a, Char) -> Bool
isPasswordComplete pw = foldl (\acc x -> acc && (snd x) /= '_') True pw
    
-- Part 1

-- Lazily calculated infinite list
findPassword :: PasswordContainer -> [Char]
findPassword pc
  | isGood    = pchar : findPassword nextPasswordContainer
  | otherwise = findPassword nextPasswordContainer
  where
    digest = generateMD5 pc
    isGood = md5IsGoodForPassword $ show digest
    pchar = passwordChar 5 digest
    nextPasswordContainer = PasswordContainer (pcId pc) (1 + (counter pc))

passwordChar :: Int -> MD5Digest -> Char
passwordChar count m = head $ drop count $ show m

md5IsGoodForPassword :: String -> Bool
md5IsGoodForPassword s = isPrefixOf "00000" s

generateMD5 :: PasswordContainer -> MD5Digest
generateMD5 pc = let
  packedId = pack $ pcId pc
  iteration = pack $ show $ counter pc
  valueToHash = append packedId iteration
  in md5 valueToHash
  
data PasswordContainer = PasswordContainer { pcId :: String, counter :: Int } deriving Show

testInput :: PasswordContainer
testInput = PasswordContainer "abc" 0

realInput :: PasswordContainer
realInput = PasswordContainer "abbhdwsy" 0
