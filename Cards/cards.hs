import Data.List
import Data.Random hiding (shuffle)
import Data.Random.Extras
import Data.Random.Source.Std

data Suit = Spade | Heart | Diamond | Club deriving (Show,Eq,Ord,Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show,Eq,Ord,Enum)
data Card = Card {rank :: Rank, suit :: Suit} deriving (Eq,Ord)
instance Show Card where
	show (Card r s) = show r ++ " of " ++ show s ++ "s"
type Deck = [Card]
type Hand = [Card]
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show)
freshDeck = [Card r s | r <- [Two .. Ace] , s <- [Spade .. Club]]

-- shuffledDeck = evalState $ runRVar (shuffle freshDeck) StdRandom    
        
getHand :: Int -> Deck -> (Hand,Deck)
getHand n deck = ((take n deck),(drop n deck))

-- evaluateHand :: Hand -> HandType
evaluateHand hand = handType
    where
        -- let (hand,_) = getHand 5 freshDeck
        sortedHand = sort hand
        straight = isStraight sortedHand
        flush = isFlush sortedHand
        royalFlush = straight && flush && ((map rank sortedHand) == [Ten .. Ace])
        freq = cardFrequency sortedHand 
        pairNums = numPairs freq 
        twoPair = (2,2) `elem` pairNums
        threeKind = (3,1) `elem` pairNums
        fullHouse = threeKind && ((2,1) `elem` pairNums)
        fourKind = (4,1) `elem` pairNums
        onePair = (2,1) `elem` pairNums
        handType
            | royalFlush = RoyalFlush
            | straight && flush = StraightFlush
            | fourKind = FourOfAKind
            | fullHouse = FullHouse
            | flush = Flush
            | straight = Straight
            | threeKind = ThreeOfAKind
            | twoPair = TwoPair
            | onePair = OnePair
            | otherwise = HighCard

        
-- Assume sorted for below.            
isStraight sortedHand = [(rank $ head sortedHand) .. (rank $ head $ reverse sortedHand) ] == (map rank sortedHand)
isFlush sortedHand = length (group $ map suit sortedHand) == 1
cardFrequency sortedHand = map (\x -> (head x,length x)) $ group $ map rank sortedHand
-- Call cardFrequency and pass output to numPairs. Pair type, number
numPairs freq =  map (\x -> (head x,length x)) $ group $ sort $ map (\x -> snd x) freq
              
isSucc a b = if succ a == b then True else False        