-- | Типи для подання тексту: символ, слово, розділовий знак, речення, текст.
module Text.Types
  ( Symbol (..)
  , Word (..)
  , Punctuation (..)
  , Token (..)
  , Sentence (..)
  , Text (..)
  , wordString
  , renderSentence
  , renderText
  , sentenceWords
  , sentenceLength
  ) where

import Prelude hiding (Word)

-- | Окремий символ тексту (літера, цифра тощо).
newtype Symbol = Symbol { symbolChar :: Char }
  deriving (Eq, Ord, Show)

-- | Слово - непорожня послідовність символів.
newtype Word = Word { wordSymbols :: [Symbol] }
  deriving (Eq, Ord, Show)

-- | Розділовий знак (крапка, кома, дужка, лапки, тире...).
newtype Punctuation = Punctuation { punctChar :: Char }
  deriving (Eq, Show)

-- | Елемент речення. Пробіл - окремий токен: після нормалізації
--   між словами завжди рівно один пробіл.
data Token
  = TWord Word
  | TPunct Punctuation
  | TSpace
  deriving (Eq, Show)

-- | Речення - послідовність токенів.
newtype Sentence = Sentence { sentenceTokens :: [Token] }
  deriving (Eq, Show)

-- | Текст - послідовність речень.
newtype Text = Text { textSentences :: [Sentence] }
  deriving (Eq, Show)

wordString :: Word -> String
wordString = map symbolChar . wordSymbols

renderToken :: Token -> String
renderToken (TWord w)                = wordString w
renderToken (TPunct (Punctuation c)) = [c]
renderToken TSpace                   = " "

renderSentence :: Sentence -> String
renderSentence = concatMap renderToken . sentenceTokens

-- | Речення розділяються одним пробілом.
renderText :: Text -> String
renderText = unwords . map renderSentence . textSentences

-- | Слова речення в порядку появи.
sentenceWords :: Sentence -> [Word]
sentenceWords s = [w | TWord w <- sentenceTokens s]

-- | Довжина речення в символах (разом із пробілами й розділовими знаками).
sentenceLength :: Sentence -> Int
sentenceLength = length . renderSentence
