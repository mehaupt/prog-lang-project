module FinalProj exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Html.Attributes exposing (..)
import String exposing (concat)
import Arithmetic

-- what started as Elm tutorial code with added reset button
-- has turned into an attempt to simulate a simple craps table
-- starting with the pass line bet and may add more later
main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


-- MODEL

init : (Model, Cmd Msg)
init =
  (initialState, Cmd.none)

type alias Model =
  { point : Int
  , cash : Int
  , firstRoll : Bool
  , pbet : Int
  , fbet : Int
  , die1 : Int
  , die2 : Int
  , toggle : Int
  }

initialState : Model
initialState =
  { point = 0
  , cash = 100
  , firstRoll = True
  , pbet = 0
  , fbet = 0
  , die1 = 0
  , die2 = 0
  , toggle = 0
  }


-- UPDATE

type Msg =
  PBet1 | PBet5 | PBet10 | PBetAll | Roll |
    NewFace (Int, Int) | Reset | FBet1 | FBet5 | FBet10 | FBetAll |
      ShowRules

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowRules ->
      let
        {toggle} = model
        tog1 = 1
        tog0 = 0
      in
        if (model.toggle == 1) then
        ({ model |
          toggle = 0
           }, Cmd.none)
        else
          ({ model |
            toggle = 1
            }, Cmd.none)
    PBet1 ->
      let
        { pbet, cash } = model
        newpBet = pbet + 1
        newCash = cash - 1

      in
        ({ model |
            pbet = newpBet,
            cash = newCash}, Cmd.none)

    PBet5 ->
      let
        { pbet, cash } = model
        newpBet = pbet + 5
        newCash = cash - 5

      in
        ({ model |
            pbet = newpBet,
            cash = newCash}, Cmd.none)

    PBet10 ->
      let
        { pbet, cash } = model
        newpBet = pbet + 10
        newCash = cash - 10

      in
        ({ model |
            pbet = newpBet,
            cash = newCash}, Cmd.none)

    PBetAll ->
      let
        { pbet, cash } = model
        newpBet = pbet + cash
        newCash = 0

      in
        ({ model |
            pbet = newpBet,
            cash = newCash}, Cmd.none)

    FBet1 ->
      let
        { fbet, cash } = model
        newfBet = fbet + 1
        newCash = cash - 1

      in
        ({ model |
            fbet = newfBet,
            cash = newCash}, Cmd.none)

    FBet5 ->
      let
        { fbet, cash } = model
        newfBet = fbet + 5
        newCash = cash - 5

      in
        ({ model |
            fbet = newfBet,
            cash = newCash}, Cmd.none)

    FBet10 ->
      let
        { fbet, cash } = model
        newfBet = fbet + 10
        newCash = cash - 10

      in
        ({ model |
            fbet = newfBet,
            cash = newCash}, Cmd.none)

    FBetAll ->
      let
        { fbet, cash } = model
        newfBet = fbet + cash
        newCash = 0

      in
        ({ model |
            fbet = newfBet,
            cash = newCash}, Cmd.none)

    Roll ->
      (model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)))

    Reset ->
      (initialState, Cmd.none)

    NewFace newface ->
      let
        (face1, face2) = newface
        rollSum = face1 + face2
        { pbet, cash, fbet } = model
        newpBet = 0
        newfBet = 0
        pwin = pbet + pbet + cash
        fwin = fbet + fbet + cash
        pfwin = pbet + pbet + fbet + fbet + cash

      in
        if (rollSum == 7 || rollSum == 11) && model.firstRoll == True then
          if (rollSum == 7) then
            ({model |
              die1 = face1,
              die2 = face2,
              pbet = newpBet,
              fbet = newfBet,
              cash = pwin,
              firstRoll = True},Cmd.none) --p bet wins, f bet loses
          else
            ({model |
              die1 = face1,
              die2 = face2,
              pbet = newpBet,
              fbet = newfBet,
              cash = pfwin,
              firstRoll = True},Cmd.none) --p bet wins, f bet wins
        else if (rollSum == 2 || rollSum == 3 || rollSum == 12) && model.firstRoll == True then
          ({model |
            die1 = face1,
            die2 = face2,
            pbet = newpBet,
            cash = fwin,
            fbet = newfBet,
            firstRoll = True}, Cmd.none) --p bet loses, f bet wins no doubles
        else if (model.firstRoll == True) then -- sets the point on the come out roll
          if (rollSum == 5 || rollSum == 6 || rollSum == 8) then -- fbet loses on these numbers
            ({model |
              die1 = face1,
              die2 = face2,
              fbet = newfBet,
              point = rollSum,
              firstRoll = False}, Cmd.none)
          else
            ({model |
              die1 = face1,
              die2 = face2,
              fbet = newfBet,
              cash = fwin,
              point = rollSum,
              firstRoll = False}, Cmd.none) -- if fbet didnt lose, then we won when we set the point

        else if (rollSum == model.point) then -- if we roll the point
          if (rollSum == 5 || rollSum == 6 || rollSum == 8) then --fbet loses on these numbers
            ({model |
              die1 = face1,
              die2 = face2,
              point = 0,
              firstRoll = True,
              cash = pwin,
              fbet = newfBet,
              pbet = newpBet}, Cmd.none) -- fbet loses, pbet wins
          else --fbet wins on other points, pbet also wins
            ({model |
              die1 = face1,
              die2 = face2,
              point = 0,
              firstRoll = True,
              cash = pfwin,
              fbet = newfBet,
              pbet = newpBet}, Cmd.none)
        else if (rollSum == 7) then -- if we roll 7 first, pbet loses, fbet loses
          ({model |
            die1 = face1,
            die2 = face2,
            point = 0,
            firstRoll = True,
            fbet = newfBet,
            pbet = newpBet}, Cmd.none)
        else -- if we dont roll 7 or the point, pbet is on hold
          if (rollSum == 5 || rollSum == 6 || rollSum == 8) then --fbet loses pbet waits
            ({model |
              die1 = face1,
              die2 = face2,
              fbet = newfBet}, Cmd.none)
          else --fbet won but pbet is still on hold
            ({model |
              die1 = face1,
              die2 = face2,
              fbet = newfBet,
              cash = fwin}, Cmd.none)



subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



--UTILITIES

createImage : Int -> String
createImage d1 =
    concat ["d", (toString d1), ".jpg"]
srules : Int -> String
srules r1 =
    concat ["rule", (toString r1), ".jpg"]



-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [ text ("***********BETS***********")]
    , div [] [ text ("Current Pass Line Bet: $" ++ (toString model.pbet)
      ++ "  Come Out Roll: " ++ (toString model.firstRoll)) ]
    , button [ onClick PBet1 ] [ text "Increase Pass Line Bet $1" ]
    , button [ onClick PBet5 ] [ text "Increase Pass Line Bet $5" ]
    , button [ onClick PBet10 ] [ text "Increase Pass Line Bet $10" ]
    , button [ onClick PBetAll ] [ text "Bet All Cash Pass Line" ]
    , div [] [text ("Current Field Bet: $" ++ (toString model.fbet))]
    , button [ onClick FBet1 ] [ text "Increase Field Bet $1" ]
    , button [ onClick FBet5 ] [ text "Increase Field Bet $5" ]
    , button [ onClick FBet10 ] [ text "Increase Field Bet $10" ]
    , button [ onClick FBetAll ] [ text "Bet All Cash Field" ]
    , div [] [ text ("-------------------------------------")]
    , img [src (createImage model.die1)] []
    , img [src (createImage model.die2)] []
    , div [] [ text ("Die Sum: " ++ (toString (model.die2 + model.die1))) ]
    , div [] [ text ("Current Point: " ++ (toString model.point)) ]
    , div [] [ text ("Current Cash: " ++ (toString model.cash))]
    , button [ onClick Roll ] [ text "Roll Die" ]
    , button [ onClick Reset ] [ text "Reset Game"]
    , div [] [ text ("-------------------------------------")]
    , button [ onClick ShowRules ] [ text "Show/Hide Rules" ]
    , div [] [ text ("-------------------------------------")]
    , img [src (srules model.toggle)] []

    ]
