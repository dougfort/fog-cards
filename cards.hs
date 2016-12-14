{-

-}
import           Control.Monad ()
import           Data.Foldable as D
import           Data.List     ()
import           Data.Maybe
import qualified Data.Sequence as SEQ
import           Text.Printf

import           Shuffle

data Suit = Clubs | Diamonds | Hearts | Spades

instance Show Suit where
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"
  show Spades   = "S"

data Rank = Ace
          | R2
          | R3
          | R4
          | R5
          | R6
          | R7
          | R8
          | R9
          | R10
          | Jack
          | Queen
          | King
  deriving (Eq)

instance Show Rank where
  show Ace   = "A"
  show R2    = "2"
  show R3    = "3"
  show R4    = "4"
  show R5    = "5"
  show R6    = "6"
  show R7    = "7"
  show R8    = "8"
  show R9    = "9"
  show R10   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"

data Card = Card Suit Rank

instance Show Card where
  show (Card s r) = show s ++ show r

-- | successor: returns true of the second card is a successor to the first
-- | does not require suits to match
succ :: Card -> Card -> Bool
succ (Card _ r1) (Card _ r2)  | r1 == R2 && r2 == Ace = True
                              | r1 == R3 && r2 == R2 = True
                              | r1 == R4 && r2 == R3 = True
                              | r1 == R5 && r2 == R4 = True
                              | r1 == R6 && r2 == R5 = True
                              | r1 == R7 && r2 == R6 = True
                              | r1 == R8 && r2 == R7 = True
                              | r1 == R9 && r2 == R8 = True
                              | r1 == R10 && r2 == R9 = True
                              | r1 == Jack && r2 == R10 = True
                              | r1 == Queen && r2 == Jack = True
                              | r1 == King && r2 == Queen = True
                              | otherwise = False

type Deck = [Card]

newdeck :: Deck
newdeck =
  [Card s r | s <- [Clubs, Diamonds, Hearts, Spades],
    r <- [ Ace
         , R2
         , R3
         , R4
         , R5
         , R6
         , R7
         , R8
         , R9
         , R10
         , Jack
         , Queen
         , King
         ]
  ]

playdecks :: [Card]
playdecks = D.concat $ Prelude.replicate 2 newdeck

-- | One column of cards, with an idex of the row where the cards start
-- | being visible.
data Stack = Stack { cards   :: SEQ.Seq Card
                   , visible :: Int
                   } deriving (Show)

-- | The array of card stacks that forms the playing area
type Tableau  = SEQ.Seq Stack

-- | Command to append part (or all) of a Stack to to another Stack
data Move = Move { sourceStack :: Int
                 , sourceIndex :: Int
                 , destStack   :: Int
                 }

instance Show Move where
  show m = "(" ++ show (sourceStack m) ++ ", " ++ show (sourceIndex m) ++ ") -> " ++
    show (destStack m)

-- | main entry point
main :: IO ()
main =
    mainloop

mainloop :: IO ()
mainloop = do (s, t) <- newgame
              playloop s t

playloop :: SEQ.Seq Card -> Tableau -> IO ()
playloop s t = do displayTableau t
                  line <- getLine
                  case words line of
                    ["quit"] -> return ()
                    ["new"] -> mainloop
                    ["deal"] -> case deal s t of
                                Left m -> do
                                  putStrLn m
                                  playloop s t
                                Right (s', t') ->
                                  playloop s' t'
                    "move":xs -> case move xs t of
                                   Nothing -> do
                                     putStrLn "invalid move"
                                     playloop s t
                                   Just t' ->
                                     playloop s t'
                    _ -> do
                      putStrLn ("unknown input: '" ++ line ++ "'")
                      playloop s t

-- | start a new game
newgame :: IO (SEQ.Seq Card, Tableau)
newgame = do
    deck <- SEQ.fromList <$> shuffle playdecks
    return (SEQ.drop startcount deck, newtableau startsizes (SEQ.take startcount deck))
  where
    startsizes = [6, 5, 5, 6, 5, 5, 6, 5, 5, 6]
    startcount = sum startsizes

newtableau :: [Int] -> SEQ.Seq Card -> Tableau
newtableau ns deck = newtableau' ns deck SEQ.empty

newtableau' :: [Int] -> SEQ.Seq Card -> Tableau -> Tableau
newtableau' [] _ t = t
newtableau' (n:ns) cs t =
  newtableau' ns (SEQ.drop n cs)  (t SEQ.|> Stack {cards=SEQ.take n cs, visible=n-1})

displayTableau :: Tableau -> IO ()
displayTableau t =
    let count = D.foldr max 0 (fmap (SEQ.length . cards) t)
        revt = SEQ.reverse t
    in do
      putStrLn "      1    2    3    4    5    6    7    8    9   10"
      putStrLn "    ================================================"
      for_ [0..count-1] (\i -> putStrLn  (printf "%3d|" (i+1) ++ D.foldr (f i) "" revt))
  where
    f i s a = a ++ showStackRow i s ++ "  "

-- | display one entry from the Stack cards item
showStackRow ::  Int -> Stack -> String
showStackRow i s | i < visible s = "..."
                 | i >= SEQ.length (cards s) = "   "
                 | otherwise = printf "%3s" $ show (SEQ.index (cards s) i)

-- | Deal out a new Card to each stack
deal :: SEQ.Seq Card -> Tableau -> Either String (SEQ.Seq Card, Tableau)
deal cs t
  | SEQ.null cs = Left "empty deck"
  | any (SEQ.null . cards) t = Left "empty column"
  | otherwise =
    let w = SEQ.length t
        ss = SEQ.zip (SEQ.take w cs) t
        cs' = SEQ.drop w cs
        f (c, s) t' = t' SEQ.|> Stack {cards=cards s SEQ.|> c, visible=visible s}
    in Right (cs', foldr f SEQ.empty ss)

move :: [String] -> Tableau -> Maybe Tableau
move xs t = do
  mc <- parseMove xs
  performMove t mc

parseMove :: [String] -> Maybe Move
parseMove xs
    | length xs == 3 =
      Just Move{sourceStack = head is - 1
               , sourceIndex = is !! 1 -1 
               , destStack = is !!2 -1
               }
    | otherwise =
      Nothing
  where
    is = mapMaybe parseInt xs

parseInt :: String -> Maybe Int
parseInt s = case reads s :: [(Int, String)] of
               [(n, "")] -> Just n
               _         -> Nothing

performMove :: Tableau -> Move -> Maybe Tableau
performMove t mc = do
  s <- t !? sourceStack mc
  d <- t !? destStack mc
  (s', m) <- cut s (sourceIndex mc)
  d' <- paste d m
  return (SEQ.update (sourceStack mc) s' $ SEQ.update (destStack mc) d' t)

-- | this is defined in a later version of the Sequence package
(!?) :: SEQ.Seq a -> Int -> Maybe a
s !? i
  | i <= 0 = Nothing
  | i >= SEQ.length s = Nothing
  | otherwise = Just (SEQ.index s i)

-- | cut a Sequence of cards from the source source stack
cut :: Stack -> Int -> Maybe (Stack, SEQ.Seq Card)
cut s i
  | i < visible s = Nothing
  | i == visible s && visible s == 0 =
    Just (Stack {cards=SEQ.empty, visible=0}, cards s)
  | i == visible s =
    let n = SEQ.length (cards s) - i in
      Just (Stack {cards=SEQ.drop n (cards s), visible=visible s-1}, SEQ.take n (cards s))
  | otherwise =
    let n = SEQ.length (cards s) - i in
      Just (Stack {cards=SEQ.drop n (cards s), visible=visible s}, SEQ.take n (cards s))

-- | paste a Sequence at the end of a stack
paste :: Stack -> SEQ.Seq Card -> Maybe Stack
paste s d = Just Stack {cards=cards s SEQ.>< d, visible=visible s}
