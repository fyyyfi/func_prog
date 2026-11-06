{-# LANGUAGE ScopedTypeVariables #-}
-- | Лабораторна робота №1, варіант 1.
--   Інформаційна система "Пакети прикладних програм, розроблених на факультеті"
--   (Haskell + MySQL, classes і instances).
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)
import Data.Proxy (Proxy (..))
import qualified Data.Text.IO as TIO
import Data.Time (Day, addDays, getCurrentTime, utctDay)
import Database.MySQL.Simple (Connection, close)
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stdin, stdout, utf8)

import Db
import Model
import Prompt

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdin, stdout]
  hSetBuffering stdout NoBuffering
  conn <- connectDb
  putStrLn "=== ІС \"Пакети прикладних програм, розроблених на факультеті\" ==="
  mainMenu conn
  close conn

-- | Пункт меню: назва і дія.
type MenuItem = (String, IO ())

-- | Показує меню, поки користувач не обере 0.
runMenu :: String -> [MenuItem] -> IO ()
runMenu title items = loop
  where
    loop = do
      putStrLn ("\n--- " ++ title ++ " ---")
      forM_ (zip [1 :: Int ..] items) $ \(i, (name, _)) -> putStrLn (show i ++ ". " ++ name)
      putStrLn "0. Назад / вихід"
      n <- askInt "Ваш вибір"
      unless (n == 0) $ do
        if n >= 1 && n <= length items
          then guarded (snd (items !! (n - 1)))
          else putStrLn "Немає такого пункту."
        loop

-- | Виконує дію, перехоплюючи помилки БД (напр. порушення зовнішнього ключа),
--   щоб програма не завершувалась.
guarded :: IO () -> IO ()
guarded act = do
  r <- try act
  case r of
    Left (e :: SomeException) -> putStrLn ("Помилка: " ++ show e)
    Right () -> pure ()

mainMenu :: Connection -> IO ()
mainMenu conn = runMenu "Головне меню"
  [ ("Пакети прикладних програм", tableMenu "Пакети"              (Proxy :: Proxy Package) conn)
  , ("Розробники (студенти, викладачі)", tableMenu "Розробники"   (Proxy :: Proxy Developer) conn)
  , ("Участь розробників у пакетах", tableMenu "Участь у пакетах" (Proxy :: Proxy PackageDeveloper) conn)
  , ("Аудиторії",                 tableMenu "Аудиторії"           (Proxy :: Proxy Room) conn)
  , ("Графік семінарів",          tableMenu "Семінари"            (Proxy :: Proxy Seminar) conn)
  , ("План демонстрацій",         tableMenu "Демонстрації"        (Proxy :: Proxy Demonstration) conn)
  , ("План тестових випробувань", tableMenu "Тестові випробування" (Proxy :: Proxy TestRun) conn)
  , ("Пошук і звіти",             reportsMenu conn)
  ]

-- | Одне й те саме меню для будь-якої таблиці завдяки класам типів.
tableMenu :: forall a. (Entity a, Display a, Editable a)
          => String -> Proxy a -> Connection -> IO ()
tableMenu title p conn = runMenu title
  [ ("Показати всі", (findAll conn :: IO [a]) >>= printAll)
  , ("Знайти за id", do
        i <- askInt "id"
        r <- findById conn i :: IO (Maybe a)
        maybe (putStrLn "Не знайдено.") (TIO.putStrLn . display) r)
  , ("Додати", do
        x  <- promptNew :: IO a
        x' <- insertEntity conn x
        putStr "Додано: " >> TIO.putStrLn (display x'))
  , ("Коригувати", do
        i <- askInt "id запису"
        r <- findById conn i :: IO (Maybe a)
        case r of
          Nothing -> putStrLn "Не знайдено."
          Just x  -> do
            putStrLn "Enter - залишити поточне значення."
            x' <- promptEdit x
            ok <- updateEntity conn x'
            putStrLn (if ok then "Оновлено." else "Змін немає."))
  , ("Видалити", do
        i  <- askInt "id запису"
        ok <- deleteEntity p conn i
        putStrLn (if ok then "Видалено." else "Не знайдено."))
  ]

reportsMenu :: Connection -> IO ()
reportsMenu conn = runMenu "Пошук і звіти"
  [ ("Пошук пакетів за ключовим словом", askText "Ключове слово" >>= searchPackages conn >>= printAll)
  , ("Пакети розробника",                askText "Фрагмент ПІБ" >>= packagesByDeveloper conn >>= printAll)
  , ("Команда пакета",                   askInt "Id пакета" >>= packageTeam conn >>= printAll)
  , ("Графік семінарів на період", do
        (from, to) <- askPeriod
        seminarSchedule conn from to >>= printAll)
  , ("План демонстрацій і випробувань на період", do
        (from, to) <- askPeriod
        demoAndTestPlan conn from to >>= printAll)
  , ("Підсумки по пакетах", packageSummary conn >>= printAll)
  ]

-- | Період; за замовчуванням - найближчі 30 днів.
askPeriod :: IO (Day, Day)
askPeriod = do
  today <- utctDay <$> getCurrentTime
  from  <- askDayDef "З дати (РРРР-ММ-ДД)" today
  to    <- askDayDef "По дату (РРРР-ММ-ДД)" (addDays 30 from)
  pure (from, to)


printAll :: Display a => [a] -> IO ()
printAll [] = putStrLn "(порожньо)"
printAll xs = mapM_ (TIO.putStrLn . display) xs
