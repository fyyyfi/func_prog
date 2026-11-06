{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Робота з MySQL: підключення, узагальнені CRUD-операції для будь-якої
--   сутності класу 'Entity', пошук і звіти.
module Db
  ( connectDb
  , findAll, findById, insertEntity, updateEntity, deleteEntity
  , searchPackages, packagesByDeveloper
  , TeamMember (..), packageTeam
  , ScheduleRow (..), seminarSchedule
  , PlanRow (..), demoAndTestPlan
  , PackageSummary (..), packageSummary
  ) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, TimeOfDay)
import Database.MySQL.Base.Types (Option (CharsetName))
import Database.MySQL.Simple
import Database.MySQL.Simple.Param (render)
import Database.MySQL.Simple.QueryResults (QueryResults (..), convertError)
import Database.MySQL.Simple.Result (convert)
import System.Environment (lookupEnv)

import Model
import Prompt (showTime, tshow)

-- | Параметри підключення беруться зі змінних середовища
--   DB_HOST, DB_PORT, DB_USER, DB_PASSWORD, DB_NAME.
connectDb :: IO Connection
connectDb = do
  host <- env "DB_HOST" "127.0.0.1"
  port <- env "DB_PORT" "3306"
  user <- env "DB_USER" "root"
  pass <- env "DB_PASSWORD" ""
  name <- env "DB_NAME" "faculty_packages"
  connect defaultConnectInfo
    { connectHost     = host
    , connectPort     = fromIntegral (read port :: Int)
    , connectUser     = user
    , connectPassword = pass
    , connectDatabase = name
    , connectOptions  = [CharsetName "utf8mb4"]
    }
  where env k d = fromMaybe d <$> lookupEnv k

-- ---------------------------------------------------------------------------
-- Узагальнені CRUD-операції: працюють для будь-якого instance Entity
-- ---------------------------------------------------------------------------

selectSql :: Entity a => Proxy a -> String
selectSql p = "SELECT id, " ++ intercalate ", " (columns p) ++ " FROM " ++ tableName p

findAll :: forall a. Entity a => Connection -> IO [a]
findAll conn = query_ conn (fromString (selectSql (Proxy :: Proxy a) ++ " ORDER BY id"))

findById :: forall a. Entity a => Connection -> Int -> IO (Maybe a)
findById conn i =
  listToMaybe <$> query conn (fromString (selectSql (Proxy :: Proxy a) ++ " WHERE id = ?")) (Only i)

-- | Додає запис і повертає його з присвоєним id.
insertEntity :: forall a. Entity a => Connection -> a -> IO a
insertEntity conn x = do
  let p    = Proxy :: Proxy a
      cols = columns p
      sql  = "INSERT INTO " ++ tableName p ++ " (" ++ intercalate ", " cols ++ ") VALUES ("
             ++ intercalate ", " (map (const "?") cols) ++ ")"
  _ <- execute conn (fromString sql) (toFields x)
  newId <- insertID conn
  pure (setId (fromIntegral newId) x)

-- | Коригування запису (за його id).
updateEntity :: forall a. Entity a => Connection -> a -> IO Bool
updateEntity conn x = do
  let p   = Proxy :: Proxy a
      sql = "UPDATE " ++ tableName p ++ " SET "
            ++ intercalate ", " [c ++ " = ?" | c <- columns p] ++ " WHERE id = ?"
  n <- execute conn (fromString sql) (toFields x ++ [render (entityId x)])
  pure (n > 0)

deleteEntity :: Entity a => Proxy a -> Connection -> Int -> IO Bool
deleteEntity p conn i = do
  n <- execute conn (fromString ("DELETE FROM " ++ tableName p ++ " WHERE id = ?")) (Only i)
  pure (n > 0)

-- ---------------------------------------------------------------------------
-- Пошук пакетів
-- ---------------------------------------------------------------------------

packageSelect :: String
packageSelect = "SELECT p.id, p.name, p.version, p.domain, p.description, p.release_date FROM packages p "

like :: Text -> Text
like t = T.concat ["%", t, "%"]

-- | Пошук за ключовим словом у назві, галузі або описі.
searchPackages :: Connection -> Text -> IO [Package]
searchPackages conn kw =
  query conn (fromString (packageSelect
    ++ "WHERE p.name LIKE ? OR p.domain LIKE ? OR p.description LIKE ? ORDER BY p.name"))
    (like kw, like kw, like kw)

-- | Пакети, над якими працює розробник із таким фрагментом ПІБ.
packagesByDeveloper :: Connection -> Text -> IO [Package]
packagesByDeveloper conn name =
  query conn (fromString (packageSelect
    ++ "JOIN package_developers pd ON pd.package_id = p.id "
    ++ "JOIN developers d ON d.id = pd.developer_id "
    ++ "WHERE d.full_name LIKE ? GROUP BY p.id ORDER BY p.name"))
    (Only (like name))

-- ---------------------------------------------------------------------------
-- Команда пакета
-- ---------------------------------------------------------------------------

data TeamMember = TeamMember
  { tmName     :: Text
  , tmCategory :: Text
  , tmUnit     :: Text
  , tmRole     :: Text
  }

instance QueryResults TeamMember where
  convertResults [fa, fb, fc, fd] [va, vb, vc, vd] =
    TeamMember (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
  convertResults fs vs = convertError fs vs 4

instance Display TeamMember where
  display m = T.concat [tmRole m, ": ", tmName m, " (", tmCategory m, ", ", tmUnit m, ")"]

packageTeam :: Connection -> Int -> IO [TeamMember]
packageTeam conn pid = query conn
  "SELECT d.full_name, d.category, d.unit, pd.role \
  \FROM package_developers pd JOIN developers d ON d.id = pd.developer_id \
  \WHERE pd.package_id = ? \
  \ORDER BY FIELD(pd.role, 'керівник', 'розробник', 'тестувальник'), d.full_name"
  (Only pid)

-- ---------------------------------------------------------------------------
-- Графік семінарів на період
-- ---------------------------------------------------------------------------

data ScheduleRow = ScheduleRow
  { srDate     :: Day
  , srStart    :: TimeOfDay
  , srDuration :: Int
  , srPackage  :: Text
  , srTopic    :: Text
  , srRoom     :: Text
  , srLeader   :: Text
  }

instance QueryResults ScheduleRow where
  convertResults [fa, fb, fc, fd, fe, ff, fg] [va, vb, vc, vd, ve, vf, vg] =
    ScheduleRow (convert fa va) (convert fb vb) (convert fc vc) (convert fd vd)
                (convert fe ve) (convert ff vf) (convert fg vg)
  convertResults fs vs = convertError fs vs 7

instance Display ScheduleRow where
  display r = T.concat
    [ tshow (srDate r), " ", showTime (srStart r), " (", tshow (srDuration r), " хв.)  "
    , srPackage r, ": ", srTopic r, "\n    ", srRoom r, ", веде ", srLeader r ]

seminarSchedule :: Connection -> Day -> Day -> IO [ScheduleRow]
seminarSchedule conn from to = query conn
  "SELECT s.seminar_date, s.start_time, s.duration_min, p.name, s.topic, \
  \CONCAT(r.name, ', ', r.building), d.full_name \
  \FROM seminars s \
  \JOIN packages p ON p.id = s.package_id \
  \JOIN rooms r ON r.id = s.room_id \
  \JOIN developers d ON d.id = s.leader_id \
  \WHERE s.seminar_date BETWEEN ? AND ? \
  \ORDER BY s.seminar_date, s.start_time"
  (from, to)

-- ---------------------------------------------------------------------------
-- План демонстрацій і тестових випробувань на період
-- ---------------------------------------------------------------------------

data PlanRow = PlanRow
  { prDate        :: Day
  , prStart       :: Maybe TimeOfDay   -- ^ у випробувань часу немає
  , prKind        :: Text
  , prPackage     :: Text
  , prDetails     :: Text
  , prResponsible :: Text
  }

instance QueryResults PlanRow where
  convertResults [fa, fb, fc, fd, fe, ff] [va, vb, vc, vd, ve, vf] =
    PlanRow (convert fa va) (convert fb vb) (convert fc vc)
            (convert fd vd) (convert fe ve) (convert ff vf)
  convertResults fs vs = convertError fs vs 6

instance Display PlanRow where
  display r = T.concat
    [ tshow (prDate r), " ", maybe "     " showTime (prStart r), "  "
    , T.justifyLeft 13 ' ' (prKind r), prPackage r, " - ", prDetails r
    , " (відп. ", prResponsible r, ")" ]

demoAndTestPlan :: Connection -> Day -> Day -> IO [PlanRow]
demoAndTestPlan conn from to = query conn
  "SELECT x.dt, x.tm, x.kind, x.pkg, x.details, x.who FROM ( \
  \  SELECT dm.demo_date AS dt, dm.start_time AS tm, 'демонстрація' AS kind, p.name AS pkg, \
  \         CONCAT('для: ', dm.audience, ', ', r.name) AS details, d.full_name AS who \
  \  FROM demonstrations dm \
  \  JOIN packages p ON p.id = dm.package_id \
  \  JOIN rooms r ON r.id = dm.room_id \
  \  JOIN developers d ON d.id = dm.presenter_id \
  \  WHERE dm.demo_date BETWEEN ? AND ? \
  \  UNION ALL \
  \  SELECT t.test_date, NULL, 'випробування', p.name, \
  \         CONCAT(t.test_kind, ' - ', t.status, '; ', t.notes), d.full_name \
  \  FROM test_runs t \
  \  JOIN packages p ON p.id = t.package_id \
  \  JOIN developers d ON d.id = t.tester_id \
  \  WHERE t.test_date BETWEEN ? AND ? \
  \) x ORDER BY x.dt, x.tm"
  (from, to, from, to)

-- ---------------------------------------------------------------------------
-- Підсумковий звіт по пакетах
-- ---------------------------------------------------------------------------

data PackageSummary = PackageSummary
  { psName        :: Text
  , psDevelopers  :: Int
  , psSeminars    :: Int
  , psDemos       :: Int
  , psTests       :: Int
  , psTestsPassed :: Int
  }

instance QueryResults PackageSummary where
  convertResults [fa, fb, fc, fd, fe, ff] [va, vb, vc, vd, ve, vf] =
    PackageSummary (convert fa va) (convert fb vb) (convert fc vc)
                   (convert fd vd) (convert fe ve) (convert ff vf)
  convertResults fs vs = convertError fs vs 6

instance Display PackageSummary where
  display s = T.concat
    [ psName s, ": розробників ", tshow (psDevelopers s), ", семінарів ", tshow (psSeminars s)
    , ", демонстрацій ", tshow (psDemos s), ", випробувань пройдено "
    , tshow (psTestsPassed s), " з ", tshow (psTests s) ]

packageSummary :: Connection -> IO [PackageSummary]
packageSummary conn = query_ conn
  "SELECT p.name, \
  \ (SELECT COUNT(*) FROM package_developers x WHERE x.package_id = p.id), \
  \ (SELECT COUNT(*) FROM seminars x WHERE x.package_id = p.id), \
  \ (SELECT COUNT(*) FROM demonstrations x WHERE x.package_id = p.id), \
  \ (SELECT COUNT(*) FROM test_runs x WHERE x.package_id = p.id), \
  \ (SELECT COUNT(*) FROM test_runs x WHERE x.package_id = p.id AND x.status = 'пройдено') \
  \FROM packages p ORDER BY p.name"
