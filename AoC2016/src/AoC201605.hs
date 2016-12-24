module AoC201605
  ( runDay,
    findPassword,
    testInput,
  ) where

import Data.ByteString.Lazy.Char8 (append, pack, unpack)
import Data.List (isPrefixOf)
import Data.Digest.Pure.MD5

runDay :: IO ()
runDay = do
  putStrLn $ "5) The password is " ++ (take 8 $ findPassword realInput ) ++ "."

-- Part 1

findPassword :: PasswordContainer -> [Char]
findPassword pc
  | isGood    = pchar : findPassword nextPasswordContainer
  | otherwise = findPassword nextPasswordContainer
  where
    digest = generateMD5 pc
    isGood = md5IsGoodForPassword $ show digest
    pchar = passwordChar digest
    nextPasswordContainer = PasswordContainer (pcId pc) (1 + (counter pc))

passwordChar :: MD5Digest -> Char
passwordChar m = head $ drop 5 $ show m

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
