module Index where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects, Never)
import RouteHash as Router exposing (HashUpdate)

type alias Model = String

type Action = NoOp | UpdateMyState String

init : (Model, Effects Action)
init =
  ("initiated index page", Effects.none)


update action model =
  case action of
    UpdateMyState newState ->
      ( model ++ newState
      , Effects.none
      )
      
    NoOp ->
      ( model, Effects.none )


view : Signal.Address Action -> Model -> Html
view address model =
  let
    th' field =
      th [] [text field]
    tr' artist =
      tr []
      [ 
      --td [] [text <| toString artist.id]
      --, td [] [text <| artist.name]
      --, td []
        --[ button [Attr.type' "button", Attr.class "btn btn-danger", onClick address (DeleteArtist artist.id)] [text "Delete"]
        --]
      ]
  in
    div [Attr.class "container"]
    [ filterForm address model
    , entryForm address model
    , table [Attr.class "table table-striped table-bordered"]
      [ thead [] [tr [] (List.map th' ["ID", "Name", "Actions"])]
      --, tbody [] (List.map tr' model.artists)
      ]
    ]


entryForm : Signal.Address Action -> Model -> Html
entryForm address model =
  div []
  [ input
    [ Attr.type' "text"
    , Attr.placeholder "Artist name..."
    --, Attr.value model.inputText
    , Attr.name "artist"
    , Attr.autofocus True
    --, onInput address UpdateInputText
    ][]
  --, button [ Attr.class "add", onClick address (CreateArtist { name = model.inputText }) ] [ text "Add" ]
  , h4 [] [text (toString model)]
  ]


filterForm : Signal.Address Action -> Model -> Html
filterForm address model =
  div [ Attr.class "search-control"]
  [ input
      [ Attr.type' "text"
      , Attr.name "search"
      --, Attr.value model.searchText
      --, onInput address UpdateSearchText
      ][]
  , span [ Attr.class "magnifying-glass" ] [ text "🔍" ]
  ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))


-- For delta2update, we provide our state as the value for the URL
delta2update : Model -> Model -> Maybe HashUpdate
delta2update previousState currentState =
  Just <| Router.set [toString currentState]


-- For location2action, we generate an action that will restore our state
location2action : List String -> List Action
location2action list =
  [(UpdateMyState "hello from pagemodule1")]
