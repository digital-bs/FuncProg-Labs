

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]


main = do
    putStrLn "Введите число:"
    input <- getLine
    let number = read input :: Int
        digitList = digits number
    putStrLn "Список цифр данного числа:"
    print digitList