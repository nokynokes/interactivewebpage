module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing ( onClick )
import Html.Lazy exposing(lazy,lazy2)
import String exposing (all, toInt,contains,isEmpty,indexes,cons)
import Char exposing (isDigit)
import Maybe exposing (..)
import List


import Bootstrap.Grid as Grid exposing (..)
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button exposing (..)

import Bootstrap.Accordion as Accordion
import Bootstrap.Card.Block as Block

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.ButtonGroup as BG




-- APP --
main : Program Never Model Msg
main =
  Html.program { init = initModel, view = view, update = update, subscriptions = (\_ -> Sub.none) }


-- MODEL --
type alias Model =
  { fooCounter : Int
  , barCounter : Int
  , fooLimit : Maybe Int
  , stateActive : Bool
  , search : String
  , searchable : String
  , occurences : List Int
  , aState : Accordion.State
  }


initModel : (Model, Cmd Msg)
initModel =
  { fooCounter = 0
  , barCounter = 0
  , fooLimit = Nothing
  , stateActive = False
  , search = ""
  , searchable = ""
  , occurences = []
  , aState = Accordion.initialState
  } ! []


-- UPDATE --
type Msg
    = NoOp String
    | Nop
    | Increment
    | Decrement
    | ToggleState
    | SearchInput String
    | SearchableInput String
    | FooLimit String
    | SearchCmd
    | AccMsg Accordion.State



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp _ ->
      (model, Cmd.none)

    Nop ->
      (model, Cmd.none)

    AccMsg state ->
      { model | aState = state } ! []

    ToggleState ->
      { model | stateActive = not model.stateActive } ! []

    Increment ->
        case model.fooLimit of
          Just limit ->
            if model.fooCounter >= limit then
              { model | barCounter = model.barCounter + 1 } ! []
            else
              { model | fooCounter = model.fooCounter + 1 } ! []
          Nothing -> { model | fooCounter = model.fooCounter + 1 } ! []

    Decrement ->
        case model.fooLimit of
          Just limit ->
            if model.fooCounter >= limit then
              { model | barCounter = model.barCounter - 1 } ! []
            else
              { model | fooCounter = model.fooCounter - 1 } ! []
          Nothing -> { model | fooCounter = model.fooCounter - 1 } ! []

    SearchInput str ->
      { model | search = str } ! []

    SearchableInput str ->
      { model | searchable = str } ! []

    FooLimit str ->
        case toInt str of
          Ok val -> {model | fooLimit = Just val } ! []
          Err _ -> (model, Cmd.none)

    SearchCmd ->
      if isEmpty model.search then
        (model, Cmd.none)
      else if isEmpty model.searchable then
        (model, Cmd.none)
      else
        let list = indexes model.search model.searchable in
          if List.isEmpty list then
            { model | occurences = [] } ! []
          else
            { model | occurences = list } ! []


-- VIEW --
searchComponents : Bool -> List Int -> Html Msg
searchComponents state occurences =
  Form.formInline []
        [ Input.text [ Input.attrs [ placeholder "Search Term" ], Input.onInput (if state then SearchInput else NoOp) ]
        , Input.text [ Input.attrs [ placeholder "Searchable Text"], Input.onInput (if state then SearchableInput else NoOp) ]
        , Button.button
            [ Button.primary
            , Button.attrs [ class "ml-sm-2 my-2" ]
            , Button.onClick (if state then SearchCmd else Nop)
            ]
            [ text "Search" ]
        , Input.text
          [ Input.attrs
              [ placeholder "Max Foo Counter"]
          , Input.onInput (if state then FooLimit else NoOp)
          ]
        , Input.text
          [ Input.attrs
              [ placeholder (List.foldl (\index acc -> acc ++ " " ++ (toString index) ) " " occurences) ]
          , Input.readonly True
          ]
        ]

toggleState : Bool -> Html Msg
toggleState state =
  Button.button [ Button.onClick ToggleState ]
        [ img
            [ src
              ( if state then
                 "static/img/pausebutton.png"
                else
                 "static/img/playbutton.png"
              )
            , height 75
            , width 75
            ] []
        ]

incrDecr : Bool -> Html Msg
incrDecr state =
  BG.buttonGroup []
      [ BG.button [ Button.onClick (if state then Increment else Nop)] [text "+"]
      , BG.button [ Button.onClick (if state then Decrement else Nop)] [text "-"]
      ]


viewState : List (String,String) -> Accordion.State -> Html Msg
viewState values state =
  Accordion.config AccMsg
        |> Accordion.withAnimation
        |> Accordion.cards
            [ Accordion.card
                { id = "debug-box"
                , options = []
                , header =
                    Accordion.header [] <| Accordion.toggle [] [ text "View State" ]
                , blocks =
                    [ Accordion.block []
                      ( List.map
                         (\(str1,str2) -> Block.text [] [ text <| str1 ++ str2 ])
                         values
                      )
                    ]
                }
            ]
        |> Accordion.view state


stateValues : Model -> List (String , String)
stateValues model =
  [ ("Foo Counter: " , toString model.fooCounter)
  , ("Foo Limit: "
    , case model.fooLimit of
        Just val -> toString val
        Nothing -> "Nothing"
    )
  , ("Bar Counter: " , toString model.barCounter)
  , ("Search Term: " , model.search)
  , ("Searchable Text: " , model.searchable)
  , ("Occurences: " , List.foldl (\index acc -> (toString index) ++ acc ++ " " ) "" model.occurences)
  , ("State Active: " , toString model.stateActive) ]

view : Model -> Html Msg
view model =
  Grid.container [ ]
      [ Grid.row [ Row.attrs [ style ( List.map (\str -> (str,"33px")) ["margin-top,margin-bottom"] ) ] ]
            [ Grid.col []
                  [ lazy2 searchComponents model.stateActive model.occurences ]
            , Grid.col []
                  [ lazy toggleState model.stateActive
                  , lazy incrDecr model.stateActive ]
            ]
      , Grid.row [ Row.attrs [ style [("margin-top","33px"),("margin-bottom","33px")] ] ]
            [ Grid.col []
              [ lazy2 viewState
                  (stateValues model)
                  model.aState
              ]
            ]
      ]
