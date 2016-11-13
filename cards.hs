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

board :: IO (Seq (Seq Card))
board = return (Data.Sequence.replicate 10 empty)

-- | main entry point
main :: IO ()
main = do
  cs <- fromList <$> shuffle playdecks
  b <- board
  print cs
  print b
