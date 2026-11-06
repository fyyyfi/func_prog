-- | Розбір сирого рядка у структуру 'Text'.
module Text.Parser
  ( normalizeSpaces
  , parseText
  ) where

import Data.Char (isAlphaNum, isSpace)
import Prelude hiding (Word)

import Text.Types

-- | Замінює табуляції, переводи рядків і послідовності пробілів одним пробілом.
normalizeSpaces :: String -> String
normalizeSpaces = unwords . words

-- | Символ, з якого складається слово.
isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

-- | Апостроф і дефіс допускаються всередині слова ("м'який", "будь-який").
isInnerChar :: Char -> Bool
isInnerChar c = c `elem` "'’ʼ-"

isSentenceEnd :: Char -> Bool
isSentenceEnd c = c `elem` ".!?"

isCloser :: Char -> Bool
isCloser c = c `elem` ")»\"”"

tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c : cs)
  | isSpace c    = TSpace : tokenize (dropWhile isSpace cs)
  | isWordChar c = let (w, rest) = spanWord s in TWord (Word (map Symbol w)) : tokenize rest
  | otherwise    = TPunct (Punctuation c) : tokenize cs

-- | Виділяє слово з урахуванням внутрішніх апострофів і дефісів.
spanWord :: String -> (String, String)
spanWord s =
  let (w, rest) = span isWordChar s
  in case rest of
       (d : r@(n : _)) | isInnerChar d && isWordChar n ->
         let (w', rest') = spanWord r in (w ++ d : w', rest')
       _ -> (w, rest)

-- | Ділить потік токенів на речення. Речення закінчується групою знаків
--   . ! ? (разом із "...", "?!") та закриваючими лапками/дужками після неї.
splitSentences :: [Token] -> [Sentence]
splitSentences = go []
  where
    go acc [] = [Sentence (trim (reverse acc)) | not (null (trim acc))]
    go acc (t : ts)
      | isEnd t =
          let (ends, rest) = span (\x -> isEnd x || isClose x) ts
          in Sentence (trim (reverse acc ++ t : ends)) : go [] rest
      | otherwise = go (t : acc) ts

    isEnd (TPunct (Punctuation c)) = isSentenceEnd c
    isEnd _                        = False
    isClose (TPunct (Punctuation c)) = isCloser c
    isClose _                        = False

    trim = dropWhile (== TSpace) . reverse . dropWhile (== TSpace) . reverse

parseText :: String -> Text
parseText = Text . splitSentences . tokenize . normalizeSpaces
