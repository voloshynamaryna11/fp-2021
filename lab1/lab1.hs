-- Лабораторна робота №1
-- студентки групи КН-32
-- підгрупи 1
-- Волошиної Марини
-- Варіант №7

-- Мета: Ознайомитись з основними типами мови. Ознайомитись зi структурою та
-- функцiями Glasgow Haskell Compiller. Набути навичок роботи з iнтерпретатором
-- ghci та визначення найпростiших функцiй.

 -- Завдання 1. Наведiть приклади виразiв вказаного типу (String,([Bool],[Integer])). 
--  Кожен список має мiстити кiлька елементiв.

-- ("abc", ([1 == 1, 0 == 1], [1..9]))
-- ("a" ++ "b", ([True, 0.1 > 0, 3 `div` 5 == 0], [-1, 0, 1]))
-- (['a'..'f'], ([True && False] ++ [True || False], [-5..0] ++ [1..5]))

-- Завдання 2. Визначте два варiанти вказаних далi функцiй, де аргументи будуть пред-
-- -- ставленi а) як один кортеж, б) без використання кортежiв чи спискiв. 
-- Функцiя приймає два числа i перевiряє, чи вони впорядкованi за зростанням.

-- a)
fun1 :: (Integer, Integer) -> Bool 
fun1 (x, y) = y >= x

-- б)
fun2 :: Integer -> Integer -> Bool 
fun2 x y = y >= x

-- Висновок: В ході виконання лабораторної роботи, я ознайомилась з основними типами мови Haskell,
-- набула навичок роботи з інтерпритатором ghci та навчилась визначати найпростіші функції 