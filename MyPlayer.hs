import SedmaBase
import SedmaGamble
import SedmaReplay
import SedmaDecks

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

{-  
    For testing, you should develop a silly player, 
    which always plays the first card from his hand. 
    The state for this player needs only to remember the current cards in hand.
-}

data SillyState = SState Hand

instance PlayerState SillyState  where
    initState p h = SState h 
    updateState t p c mc (SState s) | mc == Nothing = SState (drop 1 s)
                                    | otherwise = SState (drop 1 (s ++ maybeToList mc))

silly_player :: AIPlayer SillyState
silly_player trick (SState h) = (h !! 0)


-- Player name | Player Hand | Leader Player | Played Cards | Playing Order
data MyState = MState Player Hand Player Cards Int

{-
    InitState Parameters
    1- Player name
    2- The player's hand

    UpdateState Parameters
    1- the current trick (an ordered list of 4 cards)
    2- the leader of the current trick (the one who played the first card in this trick)
    3- the card played by this player in this trick
    4- the new card obtained at the end of this round (if any)
    5- current player state.
-}

findOrder :: Player -> Player -> Int
findOrder leader player | leader == player = 1
                        
                        | leader == A && player == B = 2
                        | leader == A && player == C = 3
                        | leader == A && player == D = 4
                        
                        | leader == B && player == C = 2
                        | leader == B && player == D = 3
                        | leader == B && player == A = 4
                        
                        | leader == C && player == D = 2
                        | leader == C && player == A = 3
                        | leader == C && player == B = 4

                        | leader == D && player == A = 2
                        | leader == D && player == B = 3
                        | leader == D && player == C = 4

instance PlayerState MyState where
    initState player hand | player == A = MState player hand A [] 1
                          | player == B = MState player hand A [] 2
                          | player == C = MState player hand A [] 3
                          | player == D = MState player hand A [] 4

    updateState trick t_leader card_played new_card (MState player hand leader cards order) = 
        MState player (dropCard (hand ++  maybeToList new_card) card_played) t_leader (cards ++ trick) (findOrder t_leader player)

dropCard :: Hand -> Card -> Hand
dropCard [] _                      = []
dropCard (y:ys) card   | y == card = dropCard ys card
                       | otherwise = y : dropCard ys card
-- includes :: Cards -> Card -> Bool
-- includes [] (Card _ rank) = False
-- includes cards (Card suit rank) | (getRank (take 1 cards) ) == rank = True
--                                 | otherwise = includes (drop 1 cards) (Card suit rank)

isBeatingHand :: Card -> Hand -> Bool
isBeatingHand leader_card hand | length hand >= 1 && (beats leader_card (hand !! 0)) = True
                               | length hand >= 2 && (beats leader_card (hand !! 1)) = True
                               | length hand >= 3 && (beats leader_card (hand !! 2)) = True 
                               | length hand == 4 && (beats leader_card (hand !! 3)) = True
                               | otherwise = False

beatingHand :: Card -> Hand -> Card
beatingHand leader_card hand | length hand >= 1 && (beats leader_card (hand !! 0)) = (hand !! 0)
                             | length hand >= 2 && (beats leader_card (hand !! 1)) = (hand !! 1)
                             | length hand >= 3 && (beats leader_card (hand !! 2)) = (hand !! 2)
                             | length hand == 4 && (beats leader_card (hand !! 3)) = (hand !! 3)
                             | otherwise = playNonSignificantCard hand

playNonSignificantCard :: Hand -> Card
playNonSignificantCard hand | length hand >= 1 && (isNonSignificant (hand !! 0)) = (hand !! 0)
                            | length hand >= 2 && (isNonSignificant (hand !! 1)) = (hand !! 1)
                            | length hand >= 3 && (isNonSignificant (hand !! 2)) = (hand !! 2)
                            | length hand == 4 && (isNonSignificant (hand !! 3)) = (hand !! 3)
                            | length hand >= 1 = (hand !! 0)

isNonSignificant :: Card -> Bool
isNonSignificant (Card _ rank) | rank == R7 || rank == RA || rank == R10 = False
                               | otherwise = True


getRank :: Card -> Rank
getRank (Card _ rank) = rank


player :: AIPlayer MyState
player trick (MState _ hand leader played_cards order) | order > 1 && length trick >= 1 && (score trick) >= 10 && (isBeatingHand (trick !! 0) hand) = beatingHand (trick !! 0) hand
                                                       | otherwise = playNonSignificantCard hand