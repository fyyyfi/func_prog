{-# LANGUAGE OverloadedStrings #-}
-- | Модель предметної області "Пакети прикладних програм, розроблених на факультеті".
--   Кожній таблиці БД відповідає окремий тип-запис, а спільна поведінка
--   (робота з БД, виведення, введення/коригування) задається класами типів
--   'Entity', 'Display' та 'Editable' та їхніми instances.
module Model
  ( Entity (..)
  , Display (..)
  , Editable (..)
  , Developer (..)
  , Package (..)
  , PackageDeveloper (..)
  , Room (..)
  , Seminar (..)
  , Demonstration (..)
  , TestRun (..)
  , developerCategories, developerRoles, testKinds, testStatuses
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, TimeOfDay)
import Database.MySQL.Simple.Param (Action, render)
import Database.MySQL.Simple.QueryResults (QueryResults (..), convertError)
import Database.MySQL.Simple.Result (convert)

import Prompt

-- ---------------------------------------------------------------------------
-- Класи типів
-- ---------------------------------------------------------------------------

-- | Сутність, що зберігається в окремій таблиці БД.
--   Перший стовпчик таблиці завжди @id@, решта перелічені в 'columns'
--   у тому ж порядку, що й значення, які повертає 'toFields'.
class QueryResults a => Entity a where
  tableName :: Proxy a -> String
  columns   :: Proxy a -> [String]
  entityId  :: a -> Int
  setId     :: Int -> a -> a
  toFields  :: a -> [Action]

-- | Людиночитне представлення запису для консолі.
class Display a where
  display :: a -> Text

-- | Введення нового запису та коригування наявного з клавіатури.
class Editable a where
  promptNew  :: IO a
  promptEdit :: a -> IO a

-- ---------------------------------------------------------------------------
-- Довідкові значення
-- ---------------------------------------------------------------------------

developerCategories, developerRoles, testKinds, testStatuses :: [Text]
developerCategories = ["студент", "викладач"]
developerRoles      = ["керівник", "розробник", "тестувальник"]
testKinds           = ["функціональні", "навантажувальні", "приймальні"]
testStatuses        = ["заплановано", "пройдено", "не пройдено"]

-- ---------------------------------------------------------------------------
-- Розробники (студенти та викладачі)
-- ---------------------------------------------------------------------------

data Developer = Developer
  { devId       :: Int
  , devName     :: Text
  , devCategory :: Text   -- ^ студент / викладач
  , devUnit     :: Text   -- ^ група (для студента) або кафедра (для викладача)
  , devEmail    :: Text
  } deriving (Show, Eq)

instance QueryResults Developer where
  convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] =
    Developer (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd) (convert fe ve)
  convertResults fs vs = convertError fs vs 5

instance Entity Developer where
  tableName _ = "developers"
  columns _   = ["full_name", "category", "unit", "email"]
  entityId    = devId
  setId i d   = d { devId = i }
  toFields d  = [render (devName d), render (devCategory d), render (devUnit d), render (devEmail d)]

instance Display Developer where
  display d = T.concat
    ["#", tshow (devId d), " ", devName d, " - ", devCategory d, ", ", devUnit d, " <", devEmail d, ">"]

instance Editable Developer where
  promptNew = Developer 0
    <$> askText  "ПІБ"
    <*> askOneOf "Категорія" developerCategories
    <*> askText  "Група / кафедра"
    <*> askText  "E-mail"
  promptEdit d = Developer (devId d)
    <$> askTextDef  "ПІБ" (devName d)
    <*> askOneOfDef "Категорія" developerCategories (devCategory d)
    <*> askTextDef  "Група / кафедра" (devUnit d)
    <*> askTextDef  "E-mail" (devEmail d)

-- ---------------------------------------------------------------------------
-- Пакети прикладних програм
-- ---------------------------------------------------------------------------

data Package = Package
  { pkgId          :: Int
  , pkgName        :: Text
  , pkgVersion     :: Text
  , pkgDomain      :: Text
  , pkgDescription :: Text
  , pkgRelease     :: Day
  } deriving (Show, Eq)

instance QueryResults Package where
  convertResults [fa, fb, fc, fd, fe, ff] [va, vb, vc, vd, ve, vf] =
    Package (convert fa va) (convert fb vb) (convert fc vc)
            (convert fd vd) (convert fe ve) (convert ff vf)
  convertResults fs vs = convertError fs vs 6

instance Entity Package where
  tableName _ = "packages"
  columns _   = ["name", "version", "domain", "description", "release_date"]
  entityId    = pkgId
  setId i p   = p { pkgId = i }
  toFields p  =
    [ render (pkgName p), render (pkgVersion p), render (pkgDomain p)
    , render (pkgDescription p), render (pkgRelease p) ]

instance Display Package where
  display p = T.concat
    [ "#", tshow (pkgId p), " ", pkgName p, " v", pkgVersion p, " [", pkgDomain p, "]"
    , "\n    ", pkgDescription p
    , "\n    Випуск: ", tshow (pkgRelease p) ]

instance Editable Package where
  promptNew = Package 0
    <$> askText "Назва пакета"
    <*> askText "Версія"
    <*> askText "Галузь застосування"
    <*> askText "Опис"
    <*> askDay  "Дата випуску (РРРР-ММ-ДД)"
  promptEdit p = Package (pkgId p)
    <$> askTextDef "Назва пакета" (pkgName p)
    <*> askTextDef "Версія" (pkgVersion p)
    <*> askTextDef "Галузь застосування" (pkgDomain p)
    <*> askTextDef "Опис" (pkgDescription p)
    <*> askDayDef  "Дата випуску (РРРР-ММ-ДД)" (pkgRelease p)

-- ---------------------------------------------------------------------------
-- Участь розробника в пакеті
-- ---------------------------------------------------------------------------

data PackageDeveloper = PackageDeveloper
  { pdId          :: Int
  , pdPackageId   :: Int
  , pdDeveloperId :: Int
  , pdRole        :: Text
  } deriving (Show, Eq)

instance QueryResults PackageDeveloper where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] =
    PackageDeveloper (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
  convertResults fs vs = convertError fs vs 4

instance Entity PackageDeveloper where
  tableName _ = "package_developers"
  columns _   = ["package_id", "developer_id", "role"]
  entityId    = pdId
  setId i p   = p { pdId = i }
  toFields p  = [render (pdPackageId p), render (pdDeveloperId p), render (pdRole p)]

instance Display PackageDeveloper where
  display p = T.concat
    [ "#", tshow (pdId p), " пакет #", tshow (pdPackageId p)
    , " <- розробник #", tshow (pdDeveloperId p), " (", pdRole p, ")" ]

instance Editable PackageDeveloper where
  promptNew = PackageDeveloper 0
    <$> askInt "Id пакета" <*> askInt "Id розробника" <*> askOneOf "Роль" developerRoles
  promptEdit p = PackageDeveloper (pdId p)
    <$> askIntDef   "Id пакета" (pdPackageId p)
    <*> askIntDef   "Id розробника" (pdDeveloperId p)
    <*> askOneOfDef "Роль" developerRoles (pdRole p)

-- ---------------------------------------------------------------------------
-- Аудиторії
-- ---------------------------------------------------------------------------

data Room = Room
  { roomId       :: Int
  , roomName     :: Text
  , roomBuilding :: Text
  , roomSeats    :: Int
  } deriving (Show, Eq)

instance QueryResults Room where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] =
    Room (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
  convertResults fs vs = convertError fs vs 4

instance Entity Room where
  tableName _ = "rooms"
  columns _   = ["name", "building", "seats"]
  entityId    = roomId
  setId i r   = r { roomId = i }
  toFields r  = [render (roomName r), render (roomBuilding r), render (roomSeats r)]

instance Display Room where
  display r = T.concat
    ["#", tshow (roomId r), " ", roomName r, ", ", roomBuilding r, ", місць: ", tshow (roomSeats r)]

instance Editable Room where
  promptNew = Room 0 <$> askText "Аудиторія" <*> askText "Корпус" <*> askInt "Кількість місць"
  promptEdit r = Room (roomId r)
    <$> askTextDef "Аудиторія" (roomName r)
    <*> askTextDef "Корпус" (roomBuilding r)
    <*> askIntDef  "Кількість місць" (roomSeats r)

-- ---------------------------------------------------------------------------
-- Графік семінарів
-- ---------------------------------------------------------------------------

data Seminar = Seminar
  { semId        :: Int
  , semPackageId :: Int
  , semRoomId    :: Int
  , semLeaderId  :: Int
  , semDate      :: Day
  , semStart     :: TimeOfDay
  , semDuration  :: Int
  , semTopic     :: Text
  } deriving (Show, Eq)

instance QueryResults Seminar where
  convertResults [fa, fb, fc, fd, fe, ff, fg, fh] [va, vb, vc, vd, ve, vf, vg, vh] =
    Seminar (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
            (convert fe ve) (convert ff vf) (convert fg vg) (convert fh vh)
  convertResults fs vs = convertError fs vs 8

instance Entity Seminar where
  tableName _ = "seminars"
  columns _   = ["package_id", "room_id", "leader_id", "seminar_date", "start_time", "duration_min", "topic"]
  entityId    = semId
  setId i s   = s { semId = i }
  toFields s  =
    [ render (semPackageId s), render (semRoomId s), render (semLeaderId s)
    , render (semDate s), render (semStart s), render (semDuration s), render (semTopic s) ]

instance Display Seminar where
  display s = T.concat
    [ "#", tshow (semId s), " ", tshow (semDate s), " ", showTime (semStart s)
    , " (", tshow (semDuration s), " хв.) \"", semTopic s, "\""
    , "\n    пакет #", tshow (semPackageId s), ", аудиторія #", tshow (semRoomId s)
    , ", веде розробник #", tshow (semLeaderId s) ]

instance Editable Seminar where
  promptNew = Seminar 0
    <$> askInt  "Id пакета"
    <*> askInt  "Id аудиторії"
    <*> askInt  "Id ведучого (розробника)"
    <*> askDay  "Дата (РРРР-ММ-ДД)"
    <*> askTime "Початок (ГГ:ХХ)"
    <*> askInt  "Тривалість, хв."
    <*> askText "Тема"
  promptEdit s = Seminar (semId s)
    <$> askIntDef  "Id пакета" (semPackageId s)
    <*> askIntDef  "Id аудиторії" (semRoomId s)
    <*> askIntDef  "Id ведучого (розробника)" (semLeaderId s)
    <*> askDayDef  "Дата (РРРР-ММ-ДД)" (semDate s)
    <*> askTimeDef "Початок (ГГ:ХХ)" (semStart s)
    <*> askIntDef  "Тривалість, хв." (semDuration s)
    <*> askTextDef "Тема" (semTopic s)

-- ---------------------------------------------------------------------------
-- План демонстрацій
-- ---------------------------------------------------------------------------

data Demonstration = Demonstration
  { demoId          :: Int
  , demoPackageId   :: Int
  , demoRoomId      :: Int
  , demoPresenterId :: Int
  , demoDate        :: Day
  , demoStart       :: TimeOfDay
  , demoAudience    :: Text
  } deriving (Show, Eq)

instance QueryResults Demonstration where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb, vc, vd, ve, vf, vg] =
    Demonstration (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
                  (convert fe ve) (convert ff vf) (convert fg vg)
  convertResults fs vs = convertError fs vs 7

instance Entity Demonstration where
  tableName _ = "demonstrations"
  columns _   = ["package_id", "room_id", "presenter_id", "demo_date", "start_time", "audience"]
  entityId    = demoId
  setId i d   = d { demoId = i }
  toFields d  =
    [ render (demoPackageId d), render (demoRoomId d), render (demoPresenterId d)
    , render (demoDate d), render (demoStart d), render (demoAudience d) ]

instance Display Demonstration where
  display d = T.concat
    [ "#", tshow (demoId d), " ", tshow (demoDate d), " ", showTime (demoStart d)
    , " пакет #", tshow (demoPackageId d), ", аудиторія #", tshow (demoRoomId d)
    , ", доповідач #", tshow (demoPresenterId d), "; для: ", demoAudience d ]

instance Editable Demonstration where
  promptNew = Demonstration 0
    <$> askInt  "Id пакета"
    <*> askInt  "Id аудиторії"
    <*> askInt  "Id доповідача (розробника)"
    <*> askDay  "Дата (РРРР-ММ-ДД)"
    <*> askTime "Початок (ГГ:ХХ)"
    <*> askText "Для кого (аудиторія слухачів)"
  promptEdit d = Demonstration (demoId d)
    <$> askIntDef  "Id пакета" (demoPackageId d)
    <*> askIntDef  "Id аудиторії" (demoRoomId d)
    <*> askIntDef  "Id доповідача (розробника)" (demoPresenterId d)
    <*> askDayDef  "Дата (РРРР-ММ-ДД)" (demoDate d)
    <*> askTimeDef "Початок (ГГ:ХХ)" (demoStart d)
    <*> askTextDef "Для кого (аудиторія слухачів)" (demoAudience d)

-- ---------------------------------------------------------------------------
-- План тестових випробувань
-- ---------------------------------------------------------------------------

data TestRun = TestRun
  { trId        :: Int
  , trPackageId :: Int
  , trTesterId  :: Int
  , trDate      :: Day
  , trKind      :: Text
  , trStatus    :: Text
  , trNotes     :: Text
  } deriving (Show, Eq)

instance QueryResults TestRun where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb, vc, vd, ve, vf, vg] =
    TestRun (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
            (convert fe ve) (convert ff vf) (convert fg vg)
  convertResults fs vs = convertError fs vs 7

instance Entity TestRun where
  tableName _ = "test_runs"
  columns _   = ["package_id", "tester_id", "test_date", "test_kind", "status", "notes"]
  entityId    = trId
  setId i t   = t { trId = i }
  toFields t  =
    [ render (trPackageId t), render (trTesterId t), render (trDate t)
    , render (trKind t), render (trStatus t), render (trNotes t) ]

instance Display TestRun where
  display t = T.concat
    [ "#", tshow (trId t), " ", tshow (trDate t), " пакет #", tshow (trPackageId t)
    , ", тестувальник #", tshow (trTesterId t), ": ", trKind t, " - ", trStatus t
    , "\n    ", trNotes t ]

instance Editable TestRun where
  promptNew = TestRun 0
    <$> askInt   "Id пакета"
    <*> askInt   "Id тестувальника (розробника)"
    <*> askDay   "Дата (РРРР-ММ-ДД)"
    <*> askOneOf "Вид випробувань" testKinds
    <*> askOneOf "Статус" testStatuses
    <*> askText  "Примітки"
  promptEdit t = TestRun (trId t)
    <$> askIntDef   "Id пакета" (trPackageId t)
    <*> askIntDef   "Id тестувальника (розробника)" (trTesterId t)
    <*> askDayDef   "Дата (РРРР-ММ-ДД)" (trDate t)
    <*> askOneOfDef "Вид випробувань" testKinds (trKind t)
    <*> askOneOfDef "Статус" testStatuses (trStatus t)
    <*> askTextDef  "Примітки" (trNotes t)
