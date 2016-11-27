{-

-}
import Control.Monad
import Data.List
import Data.Foldable as D
import qualified Data.Sequence as SEQ
import Text.Printf

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
playdecks = D.concat $ Prelude.replicate 2 newdeck

-- | One column of cards, with an idex of the row where the cards start
-- | being visible. -1 means no visible cards (empty stack)
data Stack = Stack { cards :: SEQ.Seq Card
                   , visible :: Int
                   } deriving (Show)

-- | The array of card stacks that forms the playing area
type Tableau  = SEQ.Seq Stack

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
                  case line of
                    "quit" -> return ()
                    "new" -> mainloop
                    "deal" -> do
                      (s', t') <- deal s t
                      playloop s' t'
                    _ -> do when ("move " `isPrefixOf` line)
                            t' <- move (words . stripPrefix "move " line) t
                            playloop s t'

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

deal :: SEQ.Seq Card -> Tableau -> IO (SEQ.Seq Card, Tableau)
deal s t = return (s, t)

move :: [String] -> Tableau -> IO Tableau
move xs t = return t
