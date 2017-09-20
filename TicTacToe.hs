import Data.Char
import Data.List.Split
import Text.Read

data Tile = Naught | Cross | Empty
  deriving (Eq)
type Tiles = [[ Tile ]]
data Board = Board { tiles :: Tiles, height :: Int, width :: Int }
  deriving (Show)

instance Show Tile where
  show Naught = "O"
  show Cross  = "X"
  show Empty  = " "

toString :: Board -> String
toString board
  = unlines [unwords [show tile | tile <- row] | row <- tiles board]

display :: Board -> IO ()
display = putStrLn . toString

tileAt :: Int -> Int -> Board -> Tile
tileAt y x board
  = (tiles board) !! (y - 1) !! (x - 1)

create :: Int -> Int -> Either String Board
create height width
  | height < 1 || width < 1 = Left "Board must have dimensions with at least 1!"
  | otherwise
      = Right $ Board [[ Empty | _ <- [ 1 .. width ]] | _ <- [ 1 .. height ]] height width

update :: Int -> Int -> Tile -> Board -> Either String Board
update y x tile board@(Board tiles h w)
  | y < 1 || y > height board = Left "y is out of bounds!"
  | x < 1 || x > width board  = Left "x is out of bounds!"
  | tileAt y x board /= Empty = Left "That position is not Empty!"
  | otherwise
      = Right $ Board [[ updateTile i j tile
          | (j, tile) <- zip [1..] row ]
          | (i, row) <- zip [1..] tiles ] h w
    where
      updateTile i j previousTile
        = if i == y && j == x
            then tile
            else previousTile

getXY :: IO (Int, Int)
getXY = do
  xyStr <- getLine
  let xyStrs = splitOn " " xyStr
  case map readEither xyStrs of
    [Right x, Right y] -> return (x, y)
    _ -> do
      putStrLn "Invalid x or y!"
      getXY

setupBoard :: IO Board
setupBoard = do
  putStrLn "What dimensions are the board?"
  (x, y) <- getXY
  case create y x of
    Left errMsg -> do
      putStrLn errMsg
      setupBoard
    Right board -> do
      return board

gameLoop :: Tile -> Tile -> Board -> IO ()
gameLoop currentTile nextTile board = do
  putStr "Position: "
  (x, y) <- getXY
  case update y x currentTile board of
    Left errMsg -> do
      putStrLn errMsg
      display board
      gameLoop currentTile nextTile board
    Right newBoard -> do
      display newBoard
      gameLoop nextTile currentTile newBoard
  
main = do
  board <- setupBoard 
  gameLoop Cross Naught board

