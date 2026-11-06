{-# LANGUAGE BangPatterns #-}
-- | Лабораторна робота №2.
--   Обмінне сортування зі злиттям Бетчера (Batcher's merge-exchange sort,
--   Д. Кнут, т. 3, алгоритм 5.2.2M) за допомогою k потоків.
--
--   Алгоритм складається з послідовності кроків (p, r, d). На кожному кроці
--   порівнюються та, за потреби, обмінюються пари (i, i + d) для всіх i,
--   у яких (i AND p) == r. Усі пари одного кроку не перетинаються, тому їх
--   можна обробляти незалежно: діапазон індексів ділиться на k частин, кожну
--   обробляє окремий потік (forkIO), після чого всі потоки чекають один одного
--   (бар'єр через MVar) і починається наступний крок.
--
--   Збирати з ключем -threaded (див. lab2.cabal), запускати з +RTS -N.
module Main (main) where

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM, forM_, when)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.IO (IOUArray, getElems, newListArray)
import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.List (sort)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hFlush, hSetBuffering, hSetEncoding, stdin, stdout, utf8)
import Text.Printf (printf)
import Text.Read (readMaybe)

type Arr = IOUArray Int Int

-- ---------------------------------------------------------------------------
-- Алгоритм
-- ---------------------------------------------------------------------------

-- | Усі кроки (p, r, d) алгоритму M для масиву довжини n.
--   t = ceil(log2 n); p пробігає 2^(t-1), 2^(t-2), ..., 1;
--   для кожного p: спочатку (r = 0, d = p), далі для q = 2^(t-1), 2^(t-2), ... > p:
--   (r = p, d = q - p).
batcherSteps :: Int -> [(Int, Int, Int)]
batcherSteps n
  | n < 2     = []
  | otherwise =
      [ step
      | p <- takeWhile (> 0) (iterate (`shiftR` 1) top)
      , step <- (p, 0, p) : [ (p, p, q - p) | q <- takeWhile (/= p) (iterate (`shiftR` 1) top) ]
      ]
  where
    t   = length (takeWhile (< n) (iterate (* 2) 1))   -- t = ceil(log2 n)
    top = 1 `shiftL` (t - 1)

-- | Порівняння-обмін для всіх i з [lo, hi), у яких (i AND p) == r.
compareExchangeRange :: Arr -> Int -> Int -> Int -> Int -> Int -> IO ()
compareExchangeRange arr p r d lo hi = go lo
  where
    go :: Int -> IO ()
    go !i
      | i >= hi = pure ()
      | i .&. p /= r = go (i + 1)
      | otherwise = do
          a <- unsafeRead arr i
          b <- unsafeRead arr (i + d)
          when (a > b) $ do
            unsafeWrite arr i b
            unsafeWrite arr (i + d) a
          go (i + 1)

-- | Ділить [0, m) на k приблизно рівних частин.
chunks :: Int -> Int -> [(Int, Int)]
chunks k m = [ (lo, hi) | j <- [0 .. k' - 1]
                        , let lo = j * m `div` k'
                              hi = (j + 1) * m `div` k'
                        , lo < hi ]
  where k' = max 1 (min k m)

-- | Один крок: k потоків обробляють свої частини, потім бар'єр.
parallelStep :: Int -> Arr -> Int -> (Int, Int, Int) -> IO ()
parallelStep k arr n (p, r, d)
  | k <= 1    = compareExchangeRange arr p r d 0 (n - d)
  | otherwise = do
      dones <- forM (chunks k (n - d)) $ \(lo, hi) -> do
        done <- newEmptyMVar
        _ <- forkIO (compareExchangeRange arr p r d lo hi >> putMVar done ())
        pure done
      mapM_ takeMVar dones

-- | Сортує масив довжини n у k потоків. Якщо задано trace, після кожного
--   кроку викликається з параметрами кроку (для демонстрації на малих масивах).
batcherSort :: Int -> Arr -> Int -> Maybe ((Int, Int, Int) -> IO ()) -> IO ()
batcherSort k arr n trace =
  forM_ (batcherSteps n) $ \step -> do
    parallelStep k arr n step
    maybe (pure ()) ($ step) trace

-- ---------------------------------------------------------------------------
-- Дані
-- ---------------------------------------------------------------------------

-- | Псевдовипадкові числа (xorshift), щоб не залежати від пакета random.
randomList :: Word64 -> Int -> Int -> [Int]
randomList seed range n =
  take n [ fromIntegral (x `mod` fromIntegral range) | x <- drop 1 (iterate xorshift (seed + 88172645463325252)) ]
  where
    xorshift x0 = let x1 = x0 `xor` (x0 `shiftL` 13)
                      x2 = x1 `xor` (x1 `shiftR` 7)
                  in x2 `xor` (x2 `shiftL` 17)

isSorted :: [Int] -> Bool
isSorted xs = and (zipWith (<=) xs (drop 1 xs))

-- ---------------------------------------------------------------------------
-- Діалог
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ("bench" : rest) -> bench (maybe 1000000 id (readMaybe =<< safeHead rest))
    _                -> interactive

interactive :: IO ()
interactive = do
  caps <- getNumCapabilities
  putStrLn "=== Обмінне сортування зі злиттям Бетчера у k потоків ==="
  printf "Доступно ядер (капабіліті RTS): %d\n" caps
  putStrLn "1. Ввести масив з клавіатури"
  putStrLn "2. Згенерувати випадковий масив"
  mode <- askIntIn "Ваш вибір" 1 2
  xs <- if mode == 1
          then askList
          else do
            n <- askIntIn "Розмір масиву n" 1 50000000
            pure (randomList 2026 (max 100 (n * 10)) n)
  k <- askIntIn "Кількість потоків k" 1 1024
  let n = length xs
      showArrays = n <= 40
  when showArrays $ putStrLn ("Вхідний масив:  " ++ show xs)

  arr <- newListArray (0, n - 1) xs :: IO Arr
  let trace | n <= 16 = Just $ \(p, r, d) -> do
                ys <- getElems arr
                printf "  крок p=%-3d r=%-3d d=%-3d -> %s\n" p r d (show ys)
            | otherwise = Nothing
  when (n <= 16) $ printf "Кроки алгоритму (кожен виконано в %d потоках):\n" k
  t0 <- getMonotonicTime
  batcherSort k arr n trace
  t1 <- getMonotonicTime
  ys <- getElems arr

  when showArrays $ putStrLn ("Відсортований:  " ++ show ys)
  printf "n = %d, k = %d, кроків алгоритму: %d, час: %.4f с\n" n k (length (batcherSteps n)) (t1 - t0)
  putStrLn (if isSorted ys && ys == sort xs
              then "Перевірка: масив відсортовано правильно (збігається з Data.List.sort)."
              else "Перевірка: ПОМИЛКА сортування!")

-- | Заміри часу для k = 1, 2, 4, ... на одному й тому ж масиві.
bench :: Int -> IO ()
bench n = do
  caps <- getNumCapabilities
  let xs = randomList 7 1000000000 n
      expected = sort xs
      ks = takeWhile (<= max 8 caps) (iterate (* 2) 1)
  _ <- pure $! length expected
  printf "n = %d, ядер RTS: %d, кроків: %d\n" n caps (length (batcherSteps n))
  putStrLn "| Потоків | Час, с | Прискорення | Ефективність |"
  putStrLn "|--------:|-------:|------------:|-------------:|"
  base <- timeSort 1 xs expected
  forM_ ks $ \k -> do
    t <- if k == 1 then pure base else timeSort k xs expected
    printf "| %d | %.3f | %.2fx | %.0f%% |\n" k t (base / t) (100 * base / t / fromIntegral k)

timeSort :: Int -> [Int] -> [Int] -> IO Double
timeSort k xs expected = do
  let n = length xs
  -- найкращий із трьох запусків, щоб прибрати шум
  ts <- forM [1 :: Int .. 3] $ \_ -> do
    arr <- newListArray (0, n - 1) xs :: IO Arr
    t0 <- getMonotonicTime
    batcherSort k arr n Nothing
    t1 <- getMonotonicTime
    ys <- getElems arr
    when (ys /= expected) $ fail "результат не відсортовано"
    pure (t1 - t0)
  pure (minimum ts)

-- ---------------------------------------------------------------------------
-- Введення
-- ---------------------------------------------------------------------------

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead []      = Nothing

prompt :: String -> IO String
prompt label = putStr (label ++ ": ") >> hFlush stdout >> getLine

askIntIn :: String -> Int -> Int -> IO Int
askIntIn label lo hi = do
  s <- prompt label
  case readMaybe s of
    Just v | v >= lo && v <= hi -> pure v
    _ -> printf "  Введіть ціле число від %d до %d.\n" lo hi >> askIntIn label lo hi

askList :: IO [Int]
askList = do
  s <- prompt "Елементи масиву через пробіл"
  case mapM readMaybe (words (map (\c -> if c == ',' then ' ' else c) s)) of
    Just xs@(_ : _) -> pure xs
    _ -> putStrLn "  Потрібні цілі числа через пробіл." >> askList
