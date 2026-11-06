SET NAMES utf8mb4;
-- Інформаційна система "Пакети прикладних програм, розроблених на факультеті"
-- Схема БД (MySQL 8+)

DROP DATABASE IF EXISTS faculty_packages;
CREATE DATABASE faculty_packages CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE faculty_packages;

-- Розробники ППП: студенти та викладачі факультету
CREATE TABLE developers (
    id        INT AUTO_INCREMENT PRIMARY KEY,
    full_name VARCHAR(150) NOT NULL,
    category  VARCHAR(20)  NOT NULL,   -- студент / викладач
    unit      VARCHAR(100) NOT NULL,   -- академічна група або кафедра
    email     VARCHAR(100) NOT NULL
);

-- Пакети прикладних програм
CREATE TABLE packages (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    name         VARCHAR(150) NOT NULL,
    version      VARCHAR(20)  NOT NULL,
    domain       VARCHAR(100) NOT NULL,   -- галузь застосування
    description  TEXT         NOT NULL,
    release_date DATE         NOT NULL
);

-- Хто над яким пакетом працює (багато до багатьох)
CREATE TABLE package_developers (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    package_id   INT         NOT NULL,
    developer_id INT         NOT NULL,
    role         VARCHAR(50) NOT NULL,    -- керівник / розробник / тестувальник
    UNIQUE (package_id, developer_id),
    FOREIGN KEY (package_id)   REFERENCES packages(id)   ON DELETE CASCADE,
    FOREIGN KEY (developer_id) REFERENCES developers(id) ON DELETE CASCADE
);

-- Аудиторії для семінарів і демонстрацій
CREATE TABLE rooms (
    id       INT AUTO_INCREMENT PRIMARY KEY,
    name     VARCHAR(50)  NOT NULL,
    building VARCHAR(100) NOT NULL,
    seats    INT          NOT NULL
);

-- Графік роботи семінарів для ознайомлення з ППП
CREATE TABLE seminars (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    package_id   INT          NOT NULL,
    room_id      INT          NOT NULL,
    leader_id    INT          NOT NULL,   -- хто веде семінар
    seminar_date DATE         NOT NULL,
    start_time   TIME         NOT NULL,
    duration_min INT          NOT NULL,
    topic        VARCHAR(200) NOT NULL,
    FOREIGN KEY (package_id) REFERENCES packages(id)   ON DELETE CASCADE,
    FOREIGN KEY (room_id)    REFERENCES rooms(id),
    FOREIGN KEY (leader_id)  REFERENCES developers(id)
);

-- План проведення демонстрацій
CREATE TABLE demonstrations (
    id           INT AUTO_INCREMENT PRIMARY KEY,
    package_id   INT          NOT NULL,
    room_id      INT          NOT NULL,
    presenter_id INT          NOT NULL,
    demo_date    DATE         NOT NULL,
    start_time   TIME         NOT NULL,
    audience     VARCHAR(150) NOT NULL,   -- для кого демонстрація
    FOREIGN KEY (package_id)   REFERENCES packages(id)   ON DELETE CASCADE,
    FOREIGN KEY (room_id)      REFERENCES rooms(id),
    FOREIGN KEY (presenter_id) REFERENCES developers(id)
);

-- План тестових випробувань
CREATE TABLE test_runs (
    id         INT AUTO_INCREMENT PRIMARY KEY,
    package_id INT          NOT NULL,
    tester_id  INT          NOT NULL,
    test_date  DATE         NOT NULL,
    test_kind  VARCHAR(50)  NOT NULL,     -- функціональні / навантажувальні / приймальні
    status     VARCHAR(20)  NOT NULL,     -- заплановано / пройдено / не пройдено
    notes      VARCHAR(255) NOT NULL,
    FOREIGN KEY (package_id) REFERENCES packages(id)   ON DELETE CASCADE,
    FOREIGN KEY (tester_id)  REFERENCES developers(id)
);
