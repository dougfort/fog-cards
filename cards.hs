{-

-}
import Data.Sequence

import Shuffle

data Suit = Clubs | Diamonds | Hearts | Spades

instance Show Suit where
  show Clubs = "C"
  show Diamonds = "D"
  show Hearts = "H"
  show Spades = "S"

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

instance Show Rank where
  show Ace = "A"
  show R2 = "2"
  show R3 = "3"
  show R4 = "4"
  show R5 = "5"
  show R6 = "6"
  show R7 = "7"
  show R8 = "8"
  show R9 = "9"
  show R10 = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"

data Card = Card Suit Rank

instance Show Card where
  show (Card s r) = show s ++ show r

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
playdecks = concat $ Prelude.replicate 2 newdeck

-- | One column of cards, with an idex of the row where the cards startvisible
-- | being visible. -1 means no visible cards (empty stack)
data Stack = Stack { cards :: Seq Card
                   , visible :: Int
                   } deriving (Show)

-- | The array of card stacks that forms the playing area
type Tableau  = Seq Stack

-- | main entry point
main :: IO ()
main =
    mainloop
  where
    mainloop :: IO ()
    mainloop = do b <- newgame
                  print b
                  return ()

-- | start a new game
newgame :: IO (Seq Card, Tableau)
newgame = do
    deck <- fromList <$> shuffle playdecks
    return (Data.Sequence.drop startcount deck, newtableau startsizes (Data.Sequence.take startcount deck))
  where
    startsizes = [6, 5, 5, 6, 5, 5, 6, 5, 5, 6]
    startcount = sum startsizes

newtableau :: [Int] -> Seq Card -> Tableau
newtableau ns deck = newtableau' ns deck Data.Sequence.empty

newtableau' :: [Int] -> Seq Card -> Tableau -> Tableau
newtableau' [] _ t = t
newtableau' (n:ns) cs t =
  newtableau' ns (Data.Sequence.drop n cs)  (t |> Stack {cards=Data.Sequence.take n cs, visible=n-1})
