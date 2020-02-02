module Score exposing (..)

import Elements exposing (..)
import Cards exposing (..)

--------------------------------------------------------------------------------
-- find2Max : List (Int, Int) -> (List (Int, Int), List (Int, Int))
-- Given a list of tuples of player ids and number of maki that they got in a
-- certain round, determines which ids produced the max or second max number of
-- maki.  Multiple ids could have the same number of maki, so if there is a tie
-- for first place they are all listed in the first part of the output tuple and
-- nothing in the second half. If there is a tie for second place then the
-- single first place value is given followed by the list of second place values
--------------------------------------------------------------------------------
find2Max : List (Int, Int) -> (List (Int, Int), List (Int, Int))
find2Max ps =
  case ps of
    [] -> ([], [])
    (cid, cmaki)::tl ->
      if cmaki > 0
        then case find2Max tl of
          ([(fid, fmaki)], (sid, smaki)::rest) ->
            if cmaki > fmaki
              then ([(cid, cmaki)], [(fid, fmaki)])
              else if cmaki == fmaki
                then ([(fid, fmaki), (cid, cmaki)], [])
                else if cmaki > smaki
                  then ([(fid, fmaki)], [(cid, cmaki)])
                  else if cmaki == smaki
                    then ([(fid, fmaki)], (cid, cmaki)::((sid, smaki)::rest))
                    else ([(fid, fmaki)], [(sid, smaki)])
          ((fid, fmaki)::rest, []) ->
            if cmaki > fmaki
              then ([(cid, cmaki)], (fid, fmaki)::rest)
              else if cmaki == fmaki
                then ((cid, cmaki)::((fid,fmaki)::rest), [])
                else if List.length ((fid, fmaki)::rest) == 1
                  then ((fid, fmaki)::rest, [(cid, cmaki)])
                  else ((fid, fmaki)::rest, [])
          _ -> ([(cid, cmaki)], [])
        else find2Max tl

--------------------------------------------------------------------------------
-- distribute : (List (Int, Int), List (Int, Int)) -> List Player -> List Player
-- Given the output of find2Max, it determines which players were in first
-- and second place, and assigns them their points.  First place gets six points
-- while second place gets three points.  Ties divide the number of points
-- awarded by the number of winners
--------------------------------------------------------------------------------
distribute : (List (Int, Int), List (Int, Int)) -> List Player -> List Player
distribute (fmax, smax) ps =
  let
    fPoints p =
      {p | points = (ceiling (6.0 / (fmax |> List.length |> toFloat))) + p.points}
    sPoints p =
      {p | points = (ceiling (3.0 / (smax |> List.length |> toFloat))) + p.points}
    containsMax vs f p =
      case vs of
        (id, _)::tl -> if p.id == id then f p else containsMax tl f p
        [] -> p
    -- _ = Debug.log "first" (List.map (containsFirst fmax) ps)
  in
    List.map (containsMax smax sPoints) (List.map (containsMax fmax fPoints) ps)

--------------------------------------------------------------------------------
-- setScore : Player -> Int -> Int -> Player
-- Assigns a given player their points and puddings while emptying their hand
-- and played list
--------------------------------------------------------------------------------
setScore : Player -> Int -> Int -> Player
setScore p points puddings =
  {id = p.id,
  hand = [],
  played = [],
  points = p.points + points,
  numPuds = p.numPuds + puddings}

tempPlayer1 =
  {id = 0,
  hand = [],
  played = [[Maki 3, Maki 2], [Tempura, Tempura]],
  points = 0,
  numPuds = 0}

tempPlayer2 =
  {id = 1,
  hand = [],
  played = [[Maki 2, Maki 3], [Dumpling]],
  points = 0,
  numPuds = 0}

tempPlayer3 =
  {id = 2,
  hand = [],
  played = [[Maki 2, Maki 1], [Sashimi]],
  points = 0,
  numPuds = 0}

temp : Model
temp =
  {numPlayers = 3,
  deck = [],
  player = tempPlayer1,
  opponents = [tempPlayer2, tempPlayer3],
  selected = Nothing,
  roundNum = 0}

--------------------------------------------------------------------------------
-- score : Model -> Model
-- Given the model, scores the played cards of every player and assigns them
-- their points
--------------------------------------------------------------------------------
score : Model -> Model
score m =
  let
    players = m.player :: m.opponents
    scores = List.map (\p -> let _ = Debug.log (Debug.toString p.id) (Debug.toString (calculateScore p.played)) in (p, calculateScore p.played)) players
    makiList s =
      case s of
        (p, (_, maki, _))::tl -> (p.id, maki) :: makiList tl
        [] -> []
    baseScore s =
      case s of
        (p, (points, _, puds))::tl -> setScore p points puds :: baseScore tl
        [] -> []
    maxs = scores |> makiList |> find2Max
    updatedPs = scores |> baseScore |> distribute maxs
  in
    -- let _ = Debug.log (scores |> makiList |> deckToString) maxs in
    -- let _ = Debug.log (scores |> baseScore |> deckToString) updatedPs in
    case updatedPs of
      hd::tl -> {m | player = hd, opponents = tl}
      [] -> Debug.todo "Error score: No players"

--------------------------------------------------------------------------------
-- findMax : List (Int, Int) -> List (Int, Int)
-- Finds the player or players with the maximum number of puddings
--------------------------------------------------------------------------------
findMax : List (Int, Int) -> List (Int, Int)
findMax ps =
  case ps of
    [] -> []
    (cid, cpud)::tl ->
      if cpud > 0
        then case findMax tl of
          (fid, fpud)::rest ->
            if cpud > fpud
              then [(cid, cpud)]
              else if cpud == fpud
                then (cid, cpud) :: ((fid, fpud) :: rest)
                else (fid, fpud) :: rest
          [] -> [(cid, cpud)]
        else findMax tl

--------------------------------------------------------------------------------
-- findMin : List (Int, Int) -> List (Int, Int)
-- Finds the player or players with the minimum number of puddings including 0
--------------------------------------------------------------------------------
findMin : List (Int, Int) -> List (Int, Int)
findMin ps =
  case ps of
    [] -> []
    (cid, cpud)::tl ->
      case findMin tl of
        (fid, fpud)::rest ->
          if cpud < fpud
            then [(cid, cpud)]
            else if cpud == fpud
              then (cid, cpud) :: ((fid, fpud) :: rest)
              else (fid, fpud) :: rest
        [] -> [(cid, cpud)]

--------------------------------------------------------------------------------
-- distributePuds : List Player -> List (Int, Int) -> List (Int, Int) -> List Players
-- Awards six points to all players in the max list and removes six points from
-- all players in the min points
--------------------------------------------------------------------------------
distributePuds : List Player -> List (Int, Int) -> List (Int, Int) -> List Player
distributePuds ps mins maxs =
  let
    maxPoints p =
      let points = (ceiling (6.0 / (maxs |> List.length |> toFloat))) in
      {p | points = points + p.points, numPuds = 0}
    minPoints p =
      let points = (ceiling ((6.0) / (mins |> List.length |> toFloat))) in
      {p | points = p.points - points, numPuds = 0}
    awardPoints ms f p =
      case ms of
        (pid, _)::tl -> if pid == p.id then f p else awardPoints tl f p
        [] -> p
  in List.map (awardPoints mins minPoints) (List.map (awardPoints maxs maxPoints) ps)

--------------------------------------------------------------------------------
-- findMinMax : List (Int, Int) -> (List (Int, Int), List (Int, Int))
-- Finds the min and max values for the number of puddings
--------------------------------------------------------------------------------
findMinMax : List (Int, Int) -> (List (Int, Int), List (Int, Int))
findMinMax pl =
  let
    mins = findMin pl
    (mid, _) = Maybe.withDefault (0, 0) (List.head mins)
    maxs = findMax pl
    matches (id, _) b = b || (id == mid)
    trueMaxs =
      if List.foldl matches False maxs
        then []
        else maxs
  in (mins, trueMaxs)

--------------------------------------------------------------------------------
-- finalScore : Model -> Model
-- Calculates the final score of each player and assigns them their winnings
-- from the puddings
--------------------------------------------------------------------------------
finalScore : Model -> Model
finalScore m =
  let
    players = m.player :: m.opponents
    puddingList ps =
      case ps of
        hd::tl -> (hd.id, hd.numPuds) :: puddingList tl
        [] -> []
    (mins, maxs) = findMinMax (puddingList players)
    updatedPs = distributePuds players mins maxs
    finalPs = List.map (\p -> {p | numPuds = 0}) updatedPs
  in
    case finalPs of
      hd::tl -> {m | player = hd, opponents = tl}
      [] -> Debug.todo "Error score: No players"
