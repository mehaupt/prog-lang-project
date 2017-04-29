module FinalProj exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Random

-- what started as Elm tutorial code with added reset button
-- has turned into an attempt to simulate a simple craps table
-- starting with the pass line bet and may add more later
main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL
-- When a new round starts, the first roll is called a come out Roll
--if 7 or 11 is rolled, you win
--if 2 3 12 is rolled, you lose
-- if 4,5,6,8,9,10 is rolled, you go to a point round
--if in point round, you have a point (the roll that got you to the round)
--in order to win, you must roll the point again before rolling a 7

init : (Model, Cmd Msg)
init =
  (initialState, Cmd.none)

type alias Model =
  { point : Int
  , cash : Int
  , firstRoll : Bool
  , bet : Int
  , die1 : Int
  , die2 : Int
  }

initialState : Model
initialState =
  { point = 0
  , cash = 100
  , firstRoll = True
  , bet = 0
  , die1 = 0
  , die2 = 0}


-- UPDATE

type Msg = Bet | Roll | NewFace (Int, Int) | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Bet ->
      let
        { bet, cash } = model
        newBet = bet + 1
        newCash = cash - 1

      --  newBet =
      --    if .... then
      --      bet + 1
      --    else if ... then
      --      ..
      --    else
      --      bet - 1
      in
        ({ model |
            bet = newBet,
            cash = newCash}, Cmd.none)

    Roll ->
      (model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)))

    Reset ->
      (initialState, Cmd.none)

    NewFace newface ->
      let
        (face1, face2) = newface
        rollSum = face1 + face2
        { bet, cash } = model
        newBet = 0
        newCash = cash + bet + bet

      in
        if (rollSum == 7 || rollSum == 11) && model.firstRoll == True then
            ({model |
              die1 = face1,
              die2 = face2,
              bet = newBet,
              cash = newCash,
              firstRoll = True},Cmd.none)
        else if (rollSum == 2 || rollSum == 3 || rollSum == 12) && model.firstRoll == True then
          ({model |
            die1 = face1,
            die2 = face2,
            bet = newBet,
            firstRoll = True}, Cmd.none)
        else if (model.firstRoll == True) then
          ({model |
            die1 = face1,
            die2 = face2,
            point = rollSum,
            firstRoll = False}, Cmd.none)
        else if (rollSum == model.point) then
          ({model |
            die1 = face1,
            die2 = face2,
            point = 0,
            firstRoll = True,
            cash = newCash,
            bet = newBet}, Cmd.none)
        else if (rollSum == 7) then
          ({model |
            die1 = face1,
            die2 = face2,
            point = 0,
            firstRoll = True,
            bet = newBet}, Cmd.none)
        else 
          ({model |
            die1 = face1,
            die2 = face2}, Cmd.none)




subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text ("Current Pass Line Bet: $" ++ (toString model.bet)
      ++ "  Come Out Roll: " ++ (toString model.firstRoll)) ]
    , button [ onClick Bet ] [ text "Increase Bet $1" ]
    , div [] [ text ("Die Face 1: " ++ (toString model.die1) ++
       "    Die Face 2: " ++ (toString model.die2)) ]
    , div [] [ text ("Die Sum: " ++ (toString (model.die2 + model.die1))) ]
    , div [] [ text ("Current Point: " ++ (toString model.point)) ]
    , div [] [ text ("Current Cash: " ++ (toString model.cash))]
    , button [ onClick Roll ] [ text "Roll Die" ]
    , button [ onClick Reset ] [ text "Reset Game"]
    ]
