module PageModule2 where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects, Never)
import RouteHash as Router exposing (HashUpdate)

type alias Model = String

type Action = NoOp | UpdateMyState String


init : (Model, Effects Action)
init =
  ("initiated pagemodule2", Effects.none)


update action model =
  case action of
    UpdateMyState newState ->
      ( model ++ newState
      , Effects.none
      )
      
    NoOp ->
      ( model, Effects.none )


view address model =
  h1 []
  [ text "Hello World from PageModule2!"
  , a [ Attr.href "#listing" ] [ text "Go back to user listing" ]
  , a [ Attr.href "#page-tag-1" ] [ text "Go to module 1" ]
  ]


-- For delta2update, we provide our state as the value for the URL
delta2update : Model -> Model -> Maybe HashUpdate
delta2update previousState currentState =
  Just <| Router.set [toString currentState]


-- For location2action, we generate an action that will restore our state
location2action : List String -> List Action
location2action list =
  [(UpdateMyState "hello from pagemodule1")]

