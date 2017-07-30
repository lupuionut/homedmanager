module Cmd where

import Control.Monad

type Parser a b = [a] -> ([a],b)
type Key = String
type Value = String

buildOptions :: Maybe [String] -> Maybe [(Key, Value)]
buildOptions Nothing = Nothing
buildOptions (Just xs) = pure $ map (reverseOption . takeUntil '=') xs


extractOptionValue :: Key -> Maybe [(Key,Value)] -> Maybe Value
extractOptionValue "" _ = Nothing
extractOptionValue _ Nothing = Nothing
extractOptionValue k (Just s) =
    if length filtered == 0 then Nothing else Just (snd . head $ filtered)
        where
            filtered = filter (\p -> fst p == k) s


buildCommand :: [String] -> [String]
buildCommand [] = []
buildCommand line = words $ snd $ takeUntil '-' (unwords line)


parseOptions :: [String] -> Maybe [String]
parseOptions [] = Just []
parseOptions [""] = Just []
parseOptions s = liftM2 (++) (Just([option])) (parseOptions [remain])
    where
        (remain,option) = ((findOptionStart '-')<&&>(takeUntil ' ')) (unwords s)


findOptionStart :: Char -> Parser Char Char
findOptionStart c [] = ([],' ')
findOptionStart c str
    | length remains /= 0 =
        if length remains == 0 then ([],c) else (tail(remains), c)
    | otherwise = ([],' ')
        where
            remains = dropWhile (\x -> x /= c) str


takeUntil :: Char -> Parser Char String
takeUntil separator [] = ([],"")
takeUntil separator str
    | length key /= 0 =
        if length remains == 0 then ([],key) else (tail(remains), key)
    | otherwise = ([],"")
        where
            key = takeWhile (\x -> x /= separator) str
            remains = dropWhile (\x -> x /= separator) str


(<&&>) :: Parser Char Char -> Parser Char String -> Parser Char String
(p1 <&&> p2) xs
    | length xs == 0 = ([],"")
    | otherwise = (cs2,s2)
        where
            (cs1,s1) = p1 xs
            (cs2, s2) = p2 cs1


reverseOption :: (a,a) -> (a,a)
reverseOption (a,b) = (b,a)
