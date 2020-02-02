module Adversary exposing (..)

import Elements exposing (..)
import Cards exposing (..)

opponentsTurn : Model -> List Player
opponentsTurn model =
  List.map play model.opponents

hasTopCard : Sushi -> Set -> Deck
hasTopCard sushi set =
  case set of
    [] -> []
    hd::tl ->
      case hd of
        [] -> hasTopCard sushi tl
        hd1::tl1 ->
          if hasSameColor sushi hd1
            then hd
            else hasTopCard sushi tl

chooseCard : Player -> Sushi -> Sushi -> Player
chooseCard p sushi base =
  Maybe.withDefault p (playCard sushi (hasTopCard base p.played) p)

play : Player -> Player
play p =
  if contains Sashimi p.hand
    then chooseCard p Sashimi Sashimi
    else if contains Tempura p.hand
      then chooseCard p Tempura Tempura
      else if contains Dumpling p.hand
        then chooseCard p Dumpling Dumpling
        else if contains (Nigiri Squid) p.hand
          then chooseCard p (Nigiri Squid) Wasabi
            else if contains Wasabi p.hand
              then Maybe.withDefault p (playCard Wasabi [] p)
              else if contains (Maki 3) p.hand
                then chooseCard p (Maki 3) (Maki 3)
                else if contains (Nigiri Salmon) p.hand
                  then chooseCard p (Nigiri Salmon) Wasabi
                  else if contains (Maki 2) p.hand
                    then chooseCard p (Maki 2) (Maki 2)
                    else if contains (Nigiri Egg) p.hand
                      then chooseCard p (Nigiri Egg) Wasabi
                      else if contains (Maki 1) p.hand
                        then chooseCard p (Maki 1) (Maki 1)
                        else Maybe.withDefault p (playCard (pickCard p.hand) [] p)


pickCard : Deck -> Sushi
pickCard d =
  case d of
    hd::tl -> hd
    _ -> Debug.todo "play error"
