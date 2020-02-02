module Display exposing (view)

import Elements exposing (..)
import Cards exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

red : String
red = "rgb(190,55,30)"

purple : String
purple = "rgb(190,150,180)"

blue : String
blue = "rgb(120,135,175)"

pink : String
pink = "rgb(230,180,175)"

color : String -> Html.Attribute Msg
color s = style "background-color" s

cardStyle : Sushi -> List (Html.Attribute Msg)
cardStyle s =
  [style "height" "162px",
  style "width" "100px",
  style "margin" "5px",
  style "border-radius" "15px",
  style "border-color" "white",
  style "border-width" "1px",
  style "background" (getImageURL s)]

buttonStyle : List (Html.Attribute Msg)
buttonStyle =
  [color red, style "color" "white", style "border-radius" "15px"]

deckStyle : List (Html.Attribute Msg)
deckStyle =
  [style "display" "inline", style "margin" "15px",
  color red, style "border-radius" "15px"]

createInteractCard : Sushi -> Html Msg
createInteractCard s =
  (Html.button
  (onClick (Select s) :: (cardStyle s))
  [])

displayHand : Deck -> List (Html Msg)
displayHand d =
  List.map createInteractCard d

createCard : Sushi -> Html Msg
createCard s =
  Html.button (cardStyle s) []

displayInteractDeck : Deck -> Html Msg
displayInteractDeck d =
  let deck = List.map createCard d in
  Html.button
  (onClick (Place d) :: deckStyle) deck

displayPlayed : Set -> List (Html Msg)
displayPlayed set =
  Html.button ([onClick (Place []), style "display" "inline", style "height" "162px",
  style "width" "100px", style "margin" "5px"] ++ buttonStyle)
  [Html.text "New Set"] :: (List.map displayInteractDeck set)

displayDeck : Deck -> Html Msg
displayDeck d =
  let deck = List.map createCard d in
  Html.button deckStyle deck

displayOppPlayed : List Player -> List (Html Msg)
displayOppPlayed players =
  List.map (\p -> Html.div [style "display" "block", style "margin" "30px"] (List.map displayDeck p.played)) players

resetButtonStyle : Int -> List (Html.Attribute Msg)
resetButtonStyle i =
  (onClick (Reset i)) :: (style "margin" "10px") :: buttonStyle

resetButtons : Int -> List (Html Msg)
resetButtons i =
  [Html.button (resetButtonStyle i) [Html.text "Reset"],
  Html.button (resetButtonStyle 2) [Html.text "2 Players"],
  Html.button (resetButtonStyle 3) [Html.text "3 Players"],
  Html.button (resetButtonStyle 4) [Html.text "4 Players"],
  Html.button (resetButtonStyle 5) [Html.text "5 Players"]]

opponentsScore : List Player -> List (Html Msg)
opponentsScore players =
  case players of
    [] -> []
    hd::tl ->
      (Html.div [style "background-color" "rgb(190,150,180)"]
      [(Html.text ("Opponent: " ++ (Debug.toString hd.id))),
      (Html.text (" Points: " ++ (Debug.toString hd.points))),
      (Html.text (" Puddings: " ++ (Debug.toString hd.numPuds)))])
      :: opponentsScore tl

displayScores : Model -> Html Msg
displayScores model =
  Html.div [style "background-color" purple] (Html.text
    ("You Points: " ++ (Debug.toString model.player.points) ++
    " Puddings: " ++ (Debug.toString model.player.numPuds))
    :: (opponentsScore model.opponents))

normalView : Model -> Html Msg
normalView model =
  Html.div [style "float" "center", color purple]
  [Html.div [color blue] ((resetButtons model.numPlayers) ++ [Html.div [style "display" "inline", style "float" "right", style "margin" "10px", style "color" "white"] [Html.text ("Round " ++ (Debug.toString model.roundNum))]]),
  Html.div [style "display" "block", style "margin-left" "auto", style "margin-right" "auto", color blue] (displayPlayed model.player.played),
  Html.div [style "display" "block", style "margin-left" "auto", style "margin-right" "auto", color purple] (displayHand model.player.hand),
  Html.div [style "display" "inline", color purple]
  (case model.selected of
    Nothing -> [Html.text ""]
    Just s -> [Html.text "Selected: ", createCard s]),
  displayScores model,
  Html.div [style "display" "block", style "margin-left" "auto", style "margin-right" "auto", color pink] (displayOppPlayed model.opponents),
  Html.button [style "background" "url(images/rules.jpg)", style "width" "500px", style "height" "709px"] []]

endView : Model -> Html Msg
endView model =
  Html.div [style "float" "center", color purple]
  [Html.div [color blue] (resetButtons model.numPlayers),
  (let winners = determineWinner model in
  if List.length winners == 1
    then Html.div [] [Html.text "Winner: ", let winner = Maybe.withDefault (initPlayer -1) (List.head winners) in if winner.id == 0 then Html.text "You" else Html.text ("Opponent " ++ (Debug.toString winner.id))]
    else Html.text "Tie"),
  displayScores model]

view : Model -> Html Msg
view model =
  if model.roundNum == 1 || model.roundNum == 2 || model.roundNum == 3
    then normalView model
    else endView model
