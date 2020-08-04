import SedmaDatatypes


replay :: Cards -> Maybe Winner
replay deck | length deck < 32 = Nothing
            | length deck > 32 = Nothing 
            | otherwise  = Just (helperReplay (deck, (AC, ((0, False), (0, False)))))

helperReplay :: (Cards, (Team, ((Int, Bool), (Int, Bool)))) -> Winner
helperReplay (cards, (leader, ((fScore, fBool), (sScore, sBool)))) | length cards == 0 = decide_winner ((fScore, fBool), (sScore, sBool))
                                                                   | otherwise = helperReplay (drop 4 cards, update_game (leader, (take 4 cards), ((fScore, fBool), (sScore, sBool)), (is_last_trick cards)))


update_game :: (Team, Cards, ((Int, Bool), (Int, Bool)), Bool) -> (Team, ((Int, Bool), (Int, Bool)))
update_game (leader, cards, score_board, is_last) = let (t_leader, score) = play_trick (leader, cards, is_last)
                                                    in (t_leader, update_score (t_leader, score_board, score))

update_score :: (Team, ((Int, Bool), (Int, Bool)), Int) -> ((Int, Bool), (Int, Bool))
update_score (leader, ((fScore, fBool), (sScore, sBool)), score) | leader == AC = (( (fScore + score), True), (sScore, sBool))
                                                                 | otherwise = ((fScore, fBool), ((sScore + score), True))


is_last_trick :: Cards -> Bool
is_last_trick cards = length cards == 4


decide_winner :: ((Int, Bool), (Int, Bool)) -> Winner
decide_winner ((ai, ab), (bi, bb)) | bb == False = (AC, Three)
                                   | ab == False = (BD, Three)
                                   | ai == 90 && bb == True = (AC, Two)
                                   | bi == 90 && ab == True = (BD, Two)
                                   | ai > bi = (AC, One)
                                   | otherwise = (BD, One)


play_trick :: (Team, Cards, Bool) -> (Team, Int)
play_trick (team, cards, is_last) = let 
                      card_1 = head cards
                      card_2 = head (tail cards)
                      card_3 = head (tail (tail cards))
                      card_4 = head (tail (tail (tail cards)))
                    in  if ( card_trick card_1 card_2 && not (card_trick card_1 card_3 ) ) || card_trick card_1 card_4
                          then  
                              if team == AC then (BD, count_cards ((card_1, card_2, card_3, card_4), is_last)) else (AC, count_cards ((card_1, card_2, card_3, card_4), is_last))
                          else  
                              if team == AC then (AC, count_cards ((card_1, card_2, card_3, card_4), is_last)) else (BD, count_cards ((card_1, card_2, card_3, card_4), is_last))

card_trick :: Card -> Card -> Bool
card_trick _ (Card _ R7) = True
card_trick card_1 card_2 = card_eq card_1 card_2

card_eq :: Card -> Card -> Bool
card_eq (Card _ RA) (Card _ RA)   = True
card_eq (Card _ RK) (Card _ RK)   = True
card_eq (Card _ RQ) (Card _ RQ)   = True
card_eq (Card _ RJ) (Card _ RJ)   = True
card_eq (Card _ R10) (Card _ R10) = True
card_eq (Card _ R9) (Card _ R9)   = True
card_eq (Card _ R8) (Card _ R8)   = True
card_eq (Card _ R7) (Card _ R7)   = True
card_eq _ _                       = False

card_values :: Card -> Int
card_values (Card _ RA)  = 10
card_values (Card _ R10) = 10
card_values _            = 0

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

count_cards :: ((Card, Card, Card, Card), Bool) -> Int
count_cards ((card_1, card_2, card_3, card_4), is_last) | is_last = (sumList [ (card_values card_1), (card_values card_2), (card_values card_3), (card_values card_4)]) + 10
                                                        | otherwise = sumList [ (card_values card_1), (card_values card_2), (card_values card_3), (card_values card_4)]
