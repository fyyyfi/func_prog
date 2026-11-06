-- | Лабораторна робота №3, варіант 1.
--   У кожному реченні тексту поміняти місцями перше слово з останнім,
--   не змінюючи довжини речення.
--
--   Обмінюються лише слова: розділові знаки та пробіли залишаються на своїх
--   місцях, тому довжина кожного речення не змінюється (це перевіряється).
--   Табуляції та послідовності пробілів замінюються одним пробілом.
--
--   Запуск: lab3 [вхідний файл] [вихідний файл]
--   Якщо вхідний файл не задано, шлях запитується з клавіатури.
module Main (main) where

import Control.Monad (forM_, unless)
import Prelude hiding (Word)
import System.Environment (getArgs)
import System.IO

import Text.Parser
import Text.Types

-- | Міняє місцями перше й останнє слово речення.
--   Якщо слів менше двох, речення не змінюється.
swapFirstLast :: Sentence -> Sentence
swapFirstLast s@(Sentence toks) =
  case [ k | (k, TWord _) <- indexed ] of
    (i : rest@(_ : _)) -> let j = last rest in Sentence (map (exchange i j) indexed)
    _                  -> s
  where
    indexed = zip [0 :: Int ..] toks
    -- токен на позиції i замінюється токеном з позиції j і навпаки
    exchange i j (k, t)
      | k == i    = toks !! j
      | k == j    = toks !! i
      | otherwise = t

swapInText :: Text -> Text
swapInText = Text . map swapFirstLast . textSentences

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  args <- getArgs
  inPath <- case args of
    (p : _) -> pure p
    []      -> ask "Шлях до файлу з текстом" "lab3/data/textbook.txt"
  let outPath = case args of
        (_ : o : _) -> o
        _           -> "result.txt"

  h <- openFile inPath ReadMode
  hSetEncoding h utf8
  raw <- hGetContents h
  let original = parseText raw
      result   = swapInText original
      pairs    = zip (textSentences original) (textSentences result)

  putStrLn "=== Речення до і після обміну першого й останнього слова ==="
  forM_ (zip [1 :: Int ..] pairs) $ \(i, (before, after)) -> do
    putStrLn (show i ++ ". " ++ renderSentence before)
    putStrLn (replicate (length (show i) + 2) ' ' ++ renderSentence after
              ++ "   [довжина " ++ show (sentenceLength before) ++ " -> "
              ++ show (sentenceLength after) ++ "]")

  let broken = [ i | (i, (b, a)) <- zip [1 :: Int ..] pairs, sentenceLength b /= sentenceLength a ]
  putStrLn ""
  putStrLn ("Речень: " ++ show (length pairs)
            ++ ", змінено: " ++ show (length (filter (uncurry (/=)) pairs)))
  unless (null broken) $ putStrLn ("УВАГА: змінилась довжина речень " ++ show broken)
  putStrLn (if null broken then "Довжина кожного речення збережена." else "")

  putStrLn "\n=== Результуючий текст ==="
  putStrLn (renderText result)

  withFile outPath WriteMode $ \o -> do
    hSetEncoding o utf8
    hPutStrLn o (renderText result)
  putStrLn ("\nРезультат записано у файл " ++ outPath)
  hClose h

ask :: String -> String -> IO String
ask label def = do
  putStr (label ++ " [" ++ def ++ "]: ")
  hFlush stdout
  s <- getLine
  pure (if null s then def else s)
