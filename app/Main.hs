import Data.Maybe
import Data.List
import System.IO
import Data.Char
import Data.Either
import System.Environment 

data Tape a = Tape [a] a [a] deriving(Show)

data Command = GoRight -- >
                | GoLeft -- <
                | Inc 
                | Dec
                | Print 
                | Read 
                | LeftSkobka -- [
                | RightSkobka -- ]
                     deriving Show                   

type Spisok = [Command]



parseCode :: String -> Either String Spisok
parseCode text = case parsed of
    Just code -> Right code
    Nothing -> Left "rrrrrr skobki"
    where
        parsed = check $ mapMaybe charToBf text
        charToBf x = case x of
            '>' -> Just GoRight
            '<' -> Just GoLeft
            '+' -> Just Inc
            '-' -> Just Dec
            '.' -> Just Print
            ',' -> Just Read
            '[' -> Just LeftSkobka
            ']' -> Just RightSkobka
            smth -> Nothing

check :: Spisok -> Maybe Spisok
check code = if (sum $ map checks code) == 0 then Just code else Nothing
    where
        checks x = case x of
            LeftSkobka -> 1
            RightSkobka -> -1
            _ -> 0



-- -> [0, .., 0] 0 [0, .., 0]
initialTape :: Tape Int
initialTape = Tape zeros 0 zeros
    where zeros = repeat 0

-- [2, 2, 1] 0 [1, 0, 0] -> [0, 2, 2, 1] 1 [0, 0]
moveRight :: Tape a -> Tape a
moveRight (Tape ls m (r:rs)) = Tape (m:ls) r rs

-- [2, 2, 1] 0 [1, 0, 0] -> [2, 1] 2 [0, 1, 0, 0]
moveLeft :: Tape a -> Tape a
moveLeft (Tape (l:ls) m rs) = Tape ls l (m:rs)


-- переход к следующей операции
next :: Tape Int -> Tape Command -> IO ()
next tape (Tape _ _ []) = return ()
next tape command = run tape (moveRight command)


--                          
-- [GoLeft, GoRight, Increment] -> [] GoLeft [GoRight, Increment]
runBrainfuck :: Spisok -> IO ()
runBrainfuck (x:xs) = run initialTape (commandsToTape (x:xs)) where
                      commandsToTape :: Spisok -> Tape Command
                      commandsToTape (x:xs) = Tape [] x xs
    

run :: Tape Int -> Tape Command -> IO ()
run tape command@(Tape _ GoRight _) =
    next (moveRight tape) command

run tape command@(Tape _ GoLeft _) =
    next (moveLeft tape) command

run (Tape l p r) command@(Tape _ Inc  _) =
    next (Tape l (p+1) r) command

run (Tape l p r) command@(Tape _ Dec  _) =
    next (Tape l (p-1) r) command

run tape@(Tape _ p _) command@(Tape _ Print  _) = do
    putChar (chr p)
    hFlush stdout
    next tape command

run (Tape l _ r) command@(Tape _ Read  _) = do
    m <- getChar
    next (Tape l (ord m) r) command

run tape@(Tape _ i _) command@(Tape _ LeftSkobka  _)     -- начало цикла
    | i == 0 = findEndLoop 0 tape command             -- если счетчик = 0, начинаем искать правую скобку чтобы выйти из цикла
    | otherwise = next tape command                 -- иначе переходим к выполнению команд цикла

run tape@(Tape _ i _) command@(Tape _ RightSkobka  _)     -- конец цикла
    | i /= 0 = findLoop 0 tape command             -- если счетчик не равен 0, начинаем искать соответсвующее начало цикла
    | otherwise = next tape command                 -- иначе выходим из цикла


findEndLoop :: Int -> Tape Int -> Tape Command -> IO ()                    -- поиск правой скобки
findEndLoop 1 tape command@(Tape _ RightSkobka _) = next tape command    -- ищем k=1 соответствующую правую скобку
findEndLoop k tape command@(Tape _ RightSkobka _) =                          -- счетчик k отображает количество левых скобок (начало цикла)
    findEndLoop (k-1) tape (moveRight command)                         -- если находим правую скобку (конец цикла) уменьшаем счетчик
findEndLoop k tape command@(Tape _ LeftSkobka _) =                           -- если нашли еще цикл, добавляем 1 к счетчику скобок
    findEndLoop (k+1) tape (moveRight command)                          
findEndLoop k tape command =                                            -- если попадаем на другие команды
    findEndLoop k tape (moveRight command)                              -- просто идем дальше

findLoop :: Int -> Tape Int -> Tape Command -> IO ()                    -- поиск левой скобки
findLoop 1 tape command@(Tape _ LeftSkobka _) = next tape command      -- ищем k=1 соответствующую левую скобку
findLoop k tape command@(Tape _ LeftSkobka _) =                           -- счетчик k отображает количество правых скобок
    findLoop (k-1) tape (moveLeft command)                           -- если находим левую скобку уменьшаем счетчик
findLoop k tape command@(Tape _ RightSkobka _) =
    findLoop (k+1) tape (moveLeft command)
findLoop k tape command =
    findLoop k tape (moveLeft command)


main = do
    args <- getArgs
    text <- readFile (head args)
    let res = parseCode text
    case res of
        Left str -> putStrLn "error"
        Right code -> runBrainfuck code