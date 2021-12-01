-- Лабораторна робота №3
-- студентки групи КН-32
-- підгрупи 1
-- Волошиної Марини
-- Варіант №7

-- Мета: Ознайомитись з системою типiв та класiв типiв. 
-- Набути досвiду визначення нових типiв та класiв типiв i їх використання.  

-- Завдання: Публiкацiї. Зберiгаються данi про публiкацiї, якi можуть бути 
-- книгою (автор, назва, мiсто, видавництво, рiк), 
-- статтею (автор/спiвавтори, назва статтi, назва журналу, рiк, номер журналу, сторiнки) 
-- або тезами доповiдi (автор/спiвавтори, назва доповiдi, назва конференцiї, мiсто, рiк, сторiнки). 
-- Визначне функцiю для : пошуку усiх статей (книг, тез) вказаного автора.


data BaseInfo = BaseInfo {
author :: String,
title :: String,
year :: Int   
} deriving (Show, Eq)

data Book_Info = Book_Info {
    -- bookBaseInfo :: BaseInfo,
    city :: String,
    publishing_house :: String 
} deriving (Show, Eq)

data Article_Info = Article_Info {
    -- articleBaseInfo :: BaseInfo,
    magazine_name :: String,
    magazine_number :: Int,
    article_pages :: [Int]
} deriving (Show, Eq)

data Thesis_Info = Thesis_Info {
    -- thesisBaseInfo :: BaseInfo,
    conference_title :: String,
    conference_city :: String,
    thesis_pages :: [Int]
} deriving (Show, Eq)

data Publication = Book BaseInfo Book_Info | Article BaseInfo Article_Info | Thesis BaseInfo Thesis_Info deriving (Show, Eq)



find :: [Publication] -> String -> [Publication]
find [] _ = []
find ((Book b c):xs) a
      |author b == a = (Book b c) : find xs a 
      |otherwise = find xs a
find ((Article b c):xs) a
      |author b == a = (Article b c) : find xs a 
      |otherwise = find xs a
find ((Thesis b c):xs) a
      |author b == a = (Thesis b c) : find xs a 
      |otherwise = find xs a


publications = [Book (BaseInfo "author1" "title1" 1997) (Book_Info "city" "ABC"), 
                Book (BaseInfo "author2" "title3" 1972) (Book_Info "city" "DEF"), 
                Article (BaseInfo "author1" "title5" 2001) (Article_Info "magazinename2" 101 [24..25]), 
                Article (BaseInfo "author2" "title4" 1980) (Article_Info "magazinename1" 100 [1..3]), 
                Thesis (BaseInfo "author1" "thesis1" 1999) (Thesis_Info "conference" "city" [5..10])]


-- Висновок: В ході виконання лабораторної роботи, я ознайомилась з системою типiв та класiв типiв. Набула досвiду визначення нових типiв та класiв типiв i їх використання.
