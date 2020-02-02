module Cards exposing (..)

import Random exposing (Generator)
import Random.List exposing (shuffle)

-- Possible nigiri toppings
type Topping = Salmon | Squid | Egg
-- Types of sushi
type Sushi
  = Tempura
  | Sashimi
  | Dumpling
  | Maki Int
  | Nigiri Topping
  | Pudding
  | Wasabi
  -- | Chopsticks

-- Type aliases for organizing sushi
type alias Deck = List Sushi
type alias Set = List (List Sushi)

--------------------------------------------------------------------------------
-- initDeck : Generator Deck
-- Creates a full deck of sushi and then returns a generator of it to shuffle
--------------------------------------------------------------------------------
initDeck : Generator Deck
initDeck =
  let
    many i sushi cards =
      if i > 0
        then sushi :: cards |> many (i-1) sushi
        else cards
  in
    []
    |> many 14 Tempura
    |> many 14 Sashimi
    |> many (14 + 2) Dumpling
    |> many 12 (Maki 2)
    |> many 8 (Maki 3)
    |> many 6 (Maki 1)
    |> many 10 (Nigiri Salmon)
    |> many 5 (Nigiri Squid)
    |> many 5 (Nigiri Egg)
    |> many (10 + 2) Pudding
    |> many 6 Wasabi
    -- |> many 4 Chopsticks
    |> shuffle

--------------------------------------------------------------------------------
-- deal : Int -> Deck -> (List Deck, Deck)
-- Based on the number of players in the game, deals a certain number of cards
-- into different hands for the players, then returns the hands and the
-- remainder of the deck
--------------------------------------------------------------------------------
deal : Int -> Deck -> (List Deck, Deck)
deal i deck =
  let
    numCards = 12 - i
    hands j =
      if j > 0
        then [] :: hands (j-1)
        else []
    pass hs d =
      case (hs, d) of
        ([], _) -> (hs, d)
        (_, []) -> (hs, d)
        (hd::tl, sushi::rest) ->
          let (sets, extra) = pass tl rest in
          ((sushi :: hd) :: sets, extra)
    populate j (hs, d) =
      if j > 0
        then populate (j-1) (pass hs d)
        else (hs, d)
  in populate numCards (hands i, deck)

--------------------------------------------------------------------------------
-- remove : Sushi -> Deck -> Deck
-- removes the first instance of a sushi from the deck
--------------------------------------------------------------------------------
remove : Sushi -> Deck -> Deck
remove s d =
  case d of
    [] -> []
    hd::tl -> if isEqual s hd then tl else hd :: remove s tl

--------------------------------------------------------------------------------
-- contains : Sushi -> Deck -> Bool
-- Checks to see if a given sushi appears at least once
--------------------------------------------------------------------------------
contains : Sushi -> Deck -> Bool
contains s d =
  case d of
    [] -> False
    hd::tl -> if isEqual s hd then True else contains s tl

--------------------------------------------------------------------------------
-- isEqual : Sushi -> Sushi -> Bool
-- Determines if two pieces of sushi are of the same type
--------------------------------------------------------------------------------
isEqual : Sushi -> Sushi -> Bool
isEqual s1 s2 =
  case (s1, s2) of
    (Tempura, Tempura) -> True
    (Sashimi, Sashimi) -> True
    (Dumpling, Dumpling) -> True
    (Maki i, Maki j) -> i == j
    (Nigiri a, Nigiri b) ->
      case (a, b) of
        (Salmon, Salmon) -> True
        (Squid, Squid) -> True
        (Egg, Egg) -> True
        _ -> False
    (Pudding, Pudding) -> True
    (Wasabi, Wasabi) -> True
    -- (Chopsticks, Chopsticks) -> True
    _ -> False

--------------------------------------------------------------------------------
-- hasSameColor : Sushi -> Sushi -> Bool
-- Determines if two pieces of sushi are of the same type
--------------------------------------------------------------------------------
hasSameColor : Sushi -> Sushi -> Bool
hasSameColor s1 s2 =
  case (s1, s2) of
    (Tempura, Tempura) -> True
    (Sashimi, Sashimi) -> True
    (Dumpling, Dumpling) -> True
    (Maki _, Maki _) -> True
    (Nigiri _, Nigiri _) -> True
    (Pudding, Pudding) -> True
    (Wasabi, Wasabi) -> True
    -- (Chopsticks, Chopsticks) -> True
    _ -> False
--------------------------------------------------------------------------------
-- isEqualDeck : Deck -> Deck -> Bool
-- Determines if two decks are equal in the sense that they both contain the
-- same types of sushi in the same order
--------------------------------------------------------------------------------
isEqualDeck : Deck -> Deck -> Bool
isEqualDeck d1 d2 =
  case (d1, d2) of
    ([], []) -> True
    (hd1::tl1, hd2::tl2) ->
      if isEqual hd1 hd2
        then True && isEqualDeck tl1 tl2
        else False
    _ -> False

--------------------------------------------------------------------------------
-- isValid : Sushi -> Deck -> Bool
-- Determines if a piece of sushi can be placed on a played deck
--------------------------------------------------------------------------------
isValid : Sushi -> Deck -> Bool
isValid s d =
  case (s, d) of
    (_, []) -> True
    (Tempura, Tempura::_) -> True
    (Sashimi, Sashimi::_) -> True
    (Dumpling, Dumpling::_) -> True
    (Maki _, (Maki _)::_) -> True
    (Nigiri _, Wasabi::_) -> True
    (Pudding, Pudding::_) -> True
    _ -> False

--------------------------------------------------------------------------------
-- getImageURL : Sushi -> String
-- Mappings of sushi types to image url
--------------------------------------------------------------------------------
getImageURL : Sushi -> String
getImageURL s =
  case s of
    Tempura -> "url(images/sushigo_tempura.jpg)"
    Sashimi -> "url(images/sushigo_sashimi.jpg)"
    Dumpling -> "url(images/sushigo_dumpling.jpg)"
    Maki i -> "url(images/sushigo_maki_" ++ Debug.toString i ++ ".jpg)"
    Nigiri t -> "url(images/sushigo_nigiri_" ++ Debug.toString t ++ ".jpg)"
    Pudding -> "url(images/sushigo_pudding.jpg)"
    Wasabi -> "url(images/sushigo_wasabi.jpg)"
    -- Chopsticks -> ""

--------------------------------------------------------------------------------
-- scoreTempura : Deck -> Int
-- Determines the value of a list of tempura based on the rule that each set of
-- two tempura is five points
--------------------------------------------------------------------------------
scoreTempura : Int -> Int
scoreTempura i = (i // 2) * 5

--------------------------------------------------------------------------------
-- scoreSashimi : Deck -> Int
-- Determines the value of a list of sashimi based on the rule that each set of
-- three sashimi is ten points
--------------------------------------------------------------------------------
scoreSashimi : Int -> Int
scoreSashimi i = (i // 3) * 10

--------------------------------------------------------------------------------
-- scoreDump`: Deck -> Int
-- Determines the value of a list of dumplings
--------------------------------------------------------------------------------
scoreDumpling : Int -> Int
scoreDumpling i =
  case i of
    0 -> 0
    1 -> 1
    2 -> 3
    3 -> 6
    4 -> 10
    _ -> 15

--------------------------------------------------------------------------------
-- scoreMaki : Deck -> Int
-- Determines the quantity of maki by summing the number within the deck
--------------------------------------------------------------------------------
scoreMaki : Deck -> Int
scoreMaki d =
  case d of
    (Maki v)::tl -> v + scoreMaki tl
    _ -> 0

--------------------------------------------------------------------------------
-- scoreNigiri : Deck -> Int
-- Determines the value of a list of nigiri and wasabi based on the values of
-- the toppings and the rule that these values are trippled when on top of
-- wasabi
--------------------------------------------------------------------------------
scoreNigiri : Deck -> Int
scoreNigiri d =
  case d of
    [Wasabi] -> 3
    (Nigiri Squid)::tl -> 3 * scoreNigiri tl
    (Nigiri Salmon)::tl -> 2 * scoreNigiri tl
    (Nigiri Egg)::tl -> 1 * scoreNigiri tl
    _ -> 1

--------------------------------------------------------------------------------
-- scorePudding : Deck -> Int
-- Counts the number of pudding in the deck
--------------------------------------------------------------------------------
scorePudding : Deck -> Int
scorePudding d = List.length d

--------------------------------------------------------------------------------
-- scoreChopsticks : Int
-- Chopsticks are worth no points
--------------------------------------------------------------------------------
scoreChopsticks : Int
scoreChopsticks = 0

-- temp : Set
-- temp = [[Maki 3, Maki 2, Maki 2],[Dumpling], [Nigiri Salmon]]

--------------------------------------------------------------------------------
-- calculateBaseScore : Set -> (Int, Int, Int)
-- Given the played set of a player, determines the number of base points, maki
-- played, and puddings played
--------------------------------------------------------------------------------
calculateBaseScore : Set -> (Int, Int, Int)
calculateBaseScore sets =
  case sets of
    [] -> (0, 0, 0)
    hd::tl ->
      let
        (points, maki, puds) =
          case hd of
            (Maki _)::_ -> (0, scoreMaki hd, 0)
            (Nigiri _)::_ -> (scoreNigiri hd, 0, 0)
            Pudding::_ -> (0, 0, scorePudding hd)
            _ -> (0, 0, 0)
        (extraPoints, extraMaki, extraPuds) = calculateBaseScore tl
      in (points + extraPoints, maki + extraMaki, puds + extraPuds)

--------------------------------------------------------------------------------
-- calculateScore : Set -> (Int, Int, Int)
-- Given the played set of a player, determines the number of points with
-- dumplings, maki played, and puddings played
--------------------------------------------------------------------------------
calculateScore : Set -> (Int, Int, Int)
calculateScore sets =
  let
    (points, maki, puds) = calculateBaseScore sets
    totalSets s acc =
      let (tem, sas, dum) = acc in
      case s of
        [] -> acc
        hd::tl ->
          case hd of
            Tempura::_ -> totalSets tl (tem + (List.length hd), sas, dum)
            Sashimi::_ -> totalSets tl (tem, sas + (List.length hd), dum)
            Dumpling::_ -> totalSets tl (tem, sas, dum + (List.length hd))
            _ -> totalSets tl acc
    (tempura, sashimi, dumpling) = totalSets sets (0, 0, 0)
    newPoints = points + (scoreTempura tempura) +
      (scoreSashimi sashimi) + (scoreDumpling dumpling)
  in (newPoints, maki, puds)

deckToString d =
  case d of
    [] -> ""
    hd::tl -> (Debug.toString hd ++ ", ") ++ deckToString tl
