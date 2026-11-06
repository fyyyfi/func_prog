{-# LANGUAGE OverloadedStrings #-}
-- | Допоміжні функції для діалогу з користувачем у консолі.
module Prompt
  ( tshow
  , askText, askTextDef
  , askInt, askIntDef
  , askDay, askDayDef
  , askTime, askTimeDef, showTime
  , askOneOf, askOneOfDef
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, TimeOfDay, defaultTimeLocale, parseTimeM)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

tshow :: Show a => a -> Text
tshow = T.pack . show

prompt :: String -> IO Text
prompt label = do
  putStr (label ++ ": ")
  hFlush stdout
  T.strip <$> TIO.getLine

-- | Обов'язкове текстове поле.
askText :: String -> IO Text
askText label = do
  t <- prompt label
  if T.null t then putStrLn "  Поле не може бути порожнім." >> askText label else pure t

-- | Текстове поле зі значенням за замовчуванням (Enter - залишити як є).
askTextDef :: String -> Text -> IO Text
askTextDef label def = do
  t <- prompt (label ++ " [" ++ T.unpack def ++ "]")
  pure (if T.null t then def else t)

-- | Загальна схема: читаємо рядок, розбираємо, при помилці питаємо ще раз.
askParsed :: (Text -> Maybe a) -> String -> Maybe a -> IO a
askParsed parse label def = do
  t <- prompt label
  case (T.null t, def, parse t) of
    (True, Just d, _) -> pure d
    (_, _, Just v)    -> pure v
    _                 -> putStrLn "  Некоректне значення, спробуйте ще раз." >> askParsed parse label def

withDef :: Show a => String -> a -> String
withDef label def = label ++ " [" ++ show def ++ "]"

-- | Час без секунд: 10:00.
showTime :: TimeOfDay -> Text
showTime = T.take 5 . tshow

askInt :: String -> IO Int
askInt label = askParsed (readMaybe . T.unpack) label Nothing

askIntDef :: String -> Int -> IO Int
askIntDef label def = askParsed (readMaybe . T.unpack) (withDef label def) (Just def)

parseDay :: Text -> Maybe Day
parseDay = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

askDay :: String -> IO Day
askDay label = askParsed parseDay label Nothing

askDayDef :: String -> Day -> IO Day
askDayDef label def = askParsed parseDay (withDef label def) (Just def)

-- | Час у форматі ГГ:ХХ (секунди необов'язкові).
parseTime :: Text -> Maybe TimeOfDay
parseTime t = case parse "%H:%M" of
  Just v  -> Just v
  Nothing -> parse "%H:%M:%S"
  where parse fmt = parseTimeM True defaultTimeLocale fmt (T.unpack t)

askTime :: String -> IO TimeOfDay
askTime label = askParsed parseTime label Nothing

askTimeDef :: String -> TimeOfDay -> IO TimeOfDay
askTimeDef label def = askParsed parseTime (label ++ " [" ++ T.unpack (showTime def) ++ "]") (Just def)

-- | Вибір одного значення з фіксованого списку (номером або текстом).
askOneOf :: String -> [Text] -> IO Text
askOneOf label opts = askParsed (pickOption opts) (label ++ optionsHint opts) Nothing

askOneOfDef :: String -> [Text] -> Text -> IO Text
askOneOfDef label opts def =
  askParsed (pickOption opts) (label ++ optionsHint opts ++ " [" ++ T.unpack def ++ "]") (Just def)

optionsHint :: [Text] -> String
optionsHint opts = " (" ++ T.unpack (T.intercalate ", " [tshow i <> "-" <> o | (i, o) <- zip [1 :: Int ..] opts]) ++ ")"

pickOption :: [Text] -> Text -> Maybe Text
pickOption opts t = case readMaybe (T.unpack t) of
  Just i | i >= 1 && i <= length opts -> Just (opts !! (i - 1))
  _ | T.toLower t `elem` map T.toLower opts -> Just (T.toLower t)
    | otherwise -> Nothing
