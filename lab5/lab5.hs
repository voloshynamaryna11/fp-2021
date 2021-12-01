-- Лабораторна робота №3
-- студентки групи КН-32
-- підгрупи 1
-- Волошиної Марини
-- Варіант №7

-- Мета: Ознайомитись з модульною органiзацiєю програм та засобами введення-виведення. Набути досвiду компiляцiї Haskell-програм.

-- Завдання: Реалiзувати та скомпiлювати програму, розроблену у лабораторній №3 (визначення частоти кожного елемента списку) з введенням даних: 
-- а) з клавiатури
-- б) з файлу та виведенням результатiв 
-- в) на екран
-- г) у файл

fun :: (Eq a) => [a] -> [(a, Int)]
fun []  = []
fun [x] = [(x, 1)]
fun (x:xs) = (x, count (x:xs) x): fun (deleteDup xs x) 
  where 
  count :: (Eq a) => [a] -> a -> Int 
  count [] _ = 0
  count (x:xs) a 
    |x == a = 1 + count xs a 
    |otherwise = count xs a 
  deleteDup :: (Eq a) => [a] -> a -> [a]
  deleteDup [] _ = [] 
  deleteDup (x:xs) a
    |x == a = deleteDup xs a
    |otherwise = x : deleteDup xs a 


main :: IO ()
main = do  
    putStrLn "Enter string you want to check" 

    --а) введення даних з клавiатури 
    con_input <- getLine 

    --б) введення даних з файлу
    contents <- readFile "input.txt"

    --в) виведення результатів на екран
    print(fun con_input)

    --г) виведення результатів у файл
    writeFile "output.txt" (concatMap show (fun contents))

-- Висновок: В ході виконання лабораторної роботи, я ознайомилась з модульною органiзацiєю програм та засобами введення-виведення. Також набула досвiду компiляцiї Haskell-програм.