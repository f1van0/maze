import System.IO
import System.Environment

changeLine::String -> Int -> String
changeLine mazeLine x = prevPartOfLine ++ ['X'] ++ nextPartOfLine
    where prevPartOfLine = [(mazeLine !! i) | i <- [0..(x-1)]]
          nextPartOfLine = [(mazeLine !! i) | i <- [(x+1)..(length(mazeLine) - 1)]]

changeMaze::[String] -> (Int, Int) -> [String]
changeMaze maze pos = prevPartOfMaze ++ current ++ nextPartOfMaze
    where line = snd pos
          prevPartOfMaze = [maze !! i | i<-[0..(line - 1)]]
          current = [changeLine (maze !! (line)) (fst pos)]
          nextPartOfMaze = [maze !! i | i<-[(line + 1)..(length(maze) - 1)]]

saveMaze:: [String] -> (Int, Int) -> IO ()
saveMaze maze pos = writeFile "maze.txt" (unlines(changeMaze maze pos))

reloadMaze = do maze <- readFile "maze.txt"
                lines(maze)

directionStep::(Int, Int) -> (Int, Int) -> (Int, Int)
directionStep pos dir = (x+dx, y+dy)
    where x = fst pos
          y = snd pos
          dx = fst dir
          dy = snd dir

checkDirection::[String] -> (Int, Int) -> (Int, Int) -> Bool
checkDirection maze pos dir | dir == right = if (checkHorizontal == ' ') then True
                                             else False
                            | dir == left  = if ((x > 0) && (checkHorizontal == ' ')) then True
                                             else False
                            | dir == up    = if ((y > 0) && (checkVertical == ' ')) then True
                                             else False
                            | otherwise    = if (checkVertical == ' ') then True
                                             else False
                               where x = fst pos
                                     y = snd pos
                                     
                                     checkHorizontal = maze !! y !! fst(directionStep pos dir)
                                     checkVertical = maze !! snd(directionStep pos dir) !! x

                                     right = (1, 0)
                                     left = (-1, 0)
                                     up = (0, -1)
                                     down = (0, 1)

                                     directions = [right, left, up, down]

isSameDirection::(Int, Int) -> (Int, Int) -> Bool
isSameDirection dir curdir  = if (abs(magnitude) == 2) then True
                              else False
    where magnitude = fst(directionStep dir curdir)+snd(directionStep dir curdir)

findPath::[String] -> (Int, Int) -> (Int, Int) -> IO ()
findPath maze pos dir = if ((pos == (0, 0)) && (((maze !! y) !! x) /= ' ')) then print("No way out")
                        else if (x == (length(maze !! y)-1)) then do print("Exit "++show(pos))
                        else if (y == (length maze - 1)) then do print("Exit "++show(pos))
                        else if (verticalVoid) then print("VoidExit "++show(pos))
                        else do print(show(pos)++"  "++show(dir))
                                saveMaze maze pos
                                if ((isSameDirection dir right) && (checkDirection newMaze pos right)) then findPath newMaze (directionStep pos right) right
                                else putStr("")
                                if ((isSameDirection dir left) && (checkDirection newMaze pos left)) then findPath newMaze (directionStep pos left) left
                                else putStr("")
                                if ((isSameDirection dir up) && (checkDirection newMaze pos up)) then findPath newMaze (directionStep pos left) up
                                else putStr("")
                                if ((isSameDirection dir down) && (checkDirection newMaze pos down)) then findPath newMaze (directionStep pos down) down
                                else putStr("")
                          where x = fst pos
                                y = snd pos
                                
                                newMaze = reloadMaze

                                checkHorizontal = newMaze !! y !! fst(directionStep pos dir)
                                checkVertical = newMaze !! snd(directionStep pos dir) !! x

                                verticalVoid = ((length(maze !! (y+1)) < x) || ((y > 0) && (length(maze !! (y-1)) < x)))
                              
                                right = (1, 0)
                                left  = (-1, 0)
                                up    = (0, -1)
                                down  = (0, 1)

                                directions = [right, left, up, down]
                                
main = do fileName <- getLine
          maze <- readFile fileName
          writeFile "maze.txt" maze
          findPath (lines(maze)) (0, 0) (1, 0)