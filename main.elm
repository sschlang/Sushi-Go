module Main exposing (..)

import Elements exposing (..)
import Cards exposing (..)
import Display exposing (view)
import Adversary exposing (..)
import Score exposing (score, finalScore)
import Browser
import Random

--------------------------------------------------------------------------------
-- main : Program Flags Model Msg
-- Calls Browser.element with the appropriate values
--------------------------------------------------------------------------------
main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

--------------------------------------------------------------------------------
-- subscriptions : Model -> Sub Msg
-- Required for main
--------------------------------------------------------------------------------
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

--------------------------------------------------------------------------------
-- dealHands : deck -> Model -> Model
-- Takes the deck and deals the cards out to the different players to start a
-- new round
--------------------------------------------------------------------------------
dealHands : Deck -> Model -> Model
dealHands d model =
  let
    (hands, deck) = deal model.numPlayers d
    (pHand, oHands) =
      case hands of
        hd::tl -> (hd, tl)
        [] -> Debug.todo "Error dealHands: Not enough players"
    setOpponents ds ps =
      case (ds, ps) of
        (hd1::tl1, hd2::tl2) -> resetHand hd1 hd2 :: setOpponents tl1 tl2
        _ -> []
  in
    {numPlayers = model.numPlayers,
    deck = deck,
    player = resetHand pHand model.player,
    opponents = setOpponents oHands model.opponents,
    selected = model.selected,
    roundNum = model.roundNum}

--------------------------------------------------------------------------------
-- pass : Player -> List Player -> (Player, List Player)
-- Takes the hands of each player and passes them over one player
--------------------------------------------------------------------------------
pass : Player -> List Player -> (Player, List Player)
pass p opps =
  let
    hs = p.hand :: opponentsDeck opps
    setHands hands opponents =
      case (hands, opponents) of
        ([hand], _) -> (hand, opponents)
        (hd1::tl1, hd2::tl2) ->
          let (hand, oppHands) = setHands tl1 tl2 in
          (hand, {hd2 | hand = hd1} :: oppHands)
        _ -> Debug.todo "pass error"
    (pHand, os) = setHands hs opps
  in
    ({p | hand = pHand}, os)

--------------------------------------------------------------------------------
-- takeTurn : Player -> Model -> Model
-- Sets the value of selected to Nothing and then passes the hands if there are
-- cards left or scores and deals if there are none
--------------------------------------------------------------------------------
takeTurn : Player -> Model -> Model
takeTurn p old =
  let
    model = {old | player = p, selected = Nothing}
    opps = opponentsTurn model
  in
    if List.isEmpty p.hand
      then
        let _ = Debug.log "played" model.player.played in
        let m = score model in
        -- let _ = Debug.log "deck" m.deck in
        if m.roundNum == 3
          then let _ = Debug.log "Game end" m in finalScore {m | roundNum = -1}
          else dealHands m.deck {m | roundNum = m.roundNum + 1}
      else let (player, opponents) = pass p opps in
        updatePlayers player opponents model

--------------------------------------------------------------------------------
-- placeCard : Deck -> Model -> Nodel
-- Places the selected card on the given played deck and takes turn if possible,
-- otherwise nothing happens
--------------------------------------------------------------------------------
placeCard : Deck -> Model -> Model
placeCard d model =
  case model.selected of
    Nothing -> model
    Just sushi -> case playCard sushi d model.player of
      Nothing -> model
      Just p -> takeTurn p model

--------------------------------------------------------------------------------
-- update : Msg -> Model -> (Model, Cmd Msg)
-- Receives different messages and responds appropriately to them
--------------------------------------------------------------------------------
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset i -> (initModel i, Random.generate (\d -> Shuffle d) initDeck)
    Shuffle d -> (dealHands d model, Cmd.none)
    Select s -> ({model | selected = Just s}, Cmd.none)
    Place d -> (placeCard d model, Cmd.none)
