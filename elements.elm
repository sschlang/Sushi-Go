module Elements exposing (..)

import Cards exposing (..)
import Random

-- Player model object
type alias Player =
  { id : Int
  , hand : Deck
  , played : Set
  , points : Int
  , numPuds : Int }

-- Model object
type alias Model =
  { numPlayers : Int
  , deck : Deck
  , player : Player
  , opponents : List Player
  , selected : Maybe Sushi
  , roundNum : Int }

-- Message types
type Msg = Reset Int | Shuffle Deck | Select Sushi | Place Deck

type alias Flags = ()

--------------------------------------------------------------------------------
-- init : Flags -> (Model, Cmd Msg)
-- Initializes the game with a set number of individuals
--------------------------------------------------------------------------------
init : Flags -> (Model, Cmd Msg)
init () =
  (initModel 3, Random.generate (\d -> Shuffle d) initDeck)

--------------------------------------------------------------------------------
-- initPlayer : Int -> Player
-- Initializes a player model with a given id value
--------------------------------------------------------------------------------
initPlayer : Int -> Player
initPlayer i =
  {id = i, hand = [], played = [[]], points = 0, numPuds = 0}

--------------------------------------------------------------------------------
-- initOpponents : Int -> List Player -> List Player
-- Initializes a given number of opponenents using tail recursion
--------------------------------------------------------------------------------
initOpponents : Int -> List Player -> List Player
initOpponents i acc =
  if i > 0
    then initOpponents (i-1) (initPlayer i :: acc)
    else acc

--------------------------------------------------------------------------------
-- initModel : Int -> Model
-- Initializes or reinitializes the entire model for a given number of players
--------------------------------------------------------------------------------
initModel : Int -> Model
initModel i =
  {numPlayers = i,
  deck = [],
  player = initPlayer 0,
  opponents = initOpponents (i-1) [],
  selected = Nothing,
  roundNum = 1}

--------------------------------------------------------------------------------
-- updatePlayers : Player -> List Player -> Model -> Model
-- Sets player and opponents for a given model
--------------------------------------------------------------------------------
updatePlayers : Player -> List Player -> Model -> Model
updatePlayers p opps m =
  {m | player = p, opponents = opps}

--------------------------------------------------------------------------------
-- isEqualPlayer : Player -> Player -> Bool
-- Determines if two players are the same based on their id values
--------------------------------------------------------------------------------
isEqualPlayer : Player -> Player -> Bool
isEqualPlayer p1 p2 =
  p1.id == p2.id

--------------------------------------------------------------------------------
-- opponentsDeck : List Player -> List Deck
-- Returns the list of hands for a given list of players
--------------------------------------------------------------------------------
opponentsDeck : List Player -> List Deck
opponentsDeck players =
  List.map (\p -> p.hand) players

--------------------------------------------------------------------------------
-- resetHand : Deck -> Player -> Player
-- Gives a player a new hand and sets their played to the empty list
--------------------------------------------------------------------------------
resetHand : Deck -> Player -> Player
resetHand d p =
  {p | hand = d, played = []}

--------------------------------------------------------------------------------
-- getBoard : Model -> List Set
-- Returns the list of played cards of the opponents
--------------------------------------------------------------------------------
getBoard : Model -> List Set
getBoard m =
  List.map (\p -> p.played) m.opponents

--------------------------------------------------------------------------------
-- playCard : Sushi -> Deck -> Player -> MaybePlayer
-- Checks to see if a given sushi can be applied to a given deck.  If valid, it
-- removes the sushi from the players hand, adds it to the correct deck in the
-- players played cards.  If invalid, it returns Nothing
--------------------------------------------------------------------------------
playCard : Sushi -> Deck -> Player -> Maybe Player
playCard s d p =
  let
    findValid set =
      case set of
        hd::tl ->
          if isEqualDeck hd d then if isValid s d then True else False else findValid tl
        [] -> True
    addCard old =
      case old of
        hd::tl ->
          if isEqualDeck hd d then (s::hd)::tl else hd :: addCard tl
        [] -> [[s]]
  in
    if findValid p.played
      then Just {p | hand = remove s p.hand, played = addCard p.played}
      else Nothing

--------------------------------------------------------------------------------
-- determineWinner : Model -> Player
-- Returns the player with the most points at the end of the game
--------------------------------------------------------------------------------
determineWinner : Model -> List Player
determineWinner m =
  let
    winner ps acc =
      case (ps, acc) of
        ([], _) -> acc
        (hd1::tl1, hd2::tl2) ->
          if hd1.points > hd2.points
            then winner tl1 [hd1]
            else if hd1.points == hd2.points
              then winner tl1 (hd1 :: acc)
              else winner tl1 acc
        _ -> Debug.todo "Error determineWinner: No winner"
  in winner m.opponents [m.player]
