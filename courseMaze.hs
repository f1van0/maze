import System.IO
import System.Environment

changeLine::String -> Int -> String
changeLine mazeLine x = [(mazeLine !! i) | i <- [0..(x-1)]] ++ ['x'] ++ [(mazeLine !! i) | i <- [(x+1)..(length(mazeLine) - 1)]]

changeMaze::[String] -> (Int, Int) -> [String]
changeMaze maze pos = [maze !! i | i<-[0..((snd pos) - 1)]] ++ [changeLine (maze !! (snd pos)) (fst pos)] ++ [maze !! i | i<-[((snd pos) + 1)..(length(maze) - 1)]]

directionStep::(Int, Int) -> (Int, Int) -> (Int, Int)
directionStep pos dir = (x+dx, y+dy)
    where x = fst pos
          y = snd pos
          dx = fst dir
          dy = snd dir

checkDirection::[String] -> (Int, Int) -> (Int, Int) -> IO ()
checkDirection maze pos dir | dir == right = if (checkRight == ' ') then findPath maze (stepRight, y) right
                                             else putStr("")
                            | dir == left  = if ((x > 0) && (checkLeft == ' ')) then findPath maze (stepLeft, y) left
                                             else putStr("")
                            | dir == up    = if ((y > 0) && (checkUp == ' ')) then findPath maze (x, stepUp) up
                                             else putStr("")
                            | otherwise    = if (checkDown == ' ') then findPath maze (x, stepDown) down
                                             else putStr("")
                               where x = fst pos
                                     y = snd pos

                                     stepRight = x + 1
                                     stepLeft = x - 1
                                     stepUp = y - 1
                                     stepDown = y + 1
                                     
                                     checkRight = maze !! y !! stepRight
                                     checkLeft = maze !! y !! stepLeft
                                     checkUp = maze !! stepUp !! x
                                     checkDown = maze !! stepDown !! x

                                     right = (1, 0)
                                     left = (-1, 0)
                                     up = (0, -1)
                                     down = (0, 1)

findPath::[String] -> (Int, Int) -> (Int, Int) -> IO ()
findPath maze pos dir = if (x == (length(head maze)-1)) then do print("[Exit] - "++(show pos))
                        else if (y == (length maze - 1)) then print("[Exit] - "++(show pos))
                        else if (dir == right) then do print("[Step] - "++(show pos))
                                                       if (true) then print("jhgelrg")
                                                       else print("mims")
                                                       if (false) then print("boii")
                                                       else print("memes")
                                                       checkDirection (changeMaze maze pos) pos right
                                                       checkDirection (changeMaze maze pos) pos up
                                                       checkDirection (changeMaze maze pos) pos down
                        else if (dir == left) then do  print("[Step] - "++(show pos))
                                                       checkDirection (changeMaze maze pos) pos left
                                                       checkDirection (changeMaze maze pos) pos up
                                                       checkDirection (changeMaze maze pos) pos down
                        else if (dir == up) then do    print("[Step] - "++(show pos))
                                                       checkDirection (changeMaze maze pos) pos right
                                                       checkDirection (changeMaze maze pos) pos left
                                                       checkDirection (changeMaze maze pos) pos up
                        else if (dir == down) then do  print("[Step] - "++(show pos))
                                                       checkDirection (changeMaze maze pos) pos right
                                                       checkDirection (changeMaze maze pos) pos left
                                                       checkDirection (changeMaze maze pos) pos down                              
                        else print("Error. Incorrect direction")
                          where x = fst pos
                                y = snd pos
                                
                                right = (1, 0)
                                left  = (-1, 0)
                                up    = (0, -1)
                                down  = (0, 1)
main = do fileName <- getLine
          maze <- readFile fileName
          findPath (lines(maze)) (0, 0) (1, 0)