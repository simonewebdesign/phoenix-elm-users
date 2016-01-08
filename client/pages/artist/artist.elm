module Artist where

import Debug
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen)


-- MODEL

type alias Model =
  { inputText : String
  , artists : List Artist
  }

type alias Artist =
  { id: Int
  , name: String
  }


--artist : Json.Decode.Decoder Artist
decoder =
  let
    artist =
      Json.Decode.object2 Artist
        ("id" := Json.Decode.int)
        ("name" := Json.Decode.string)
  in
    "data" := Json.Decode.list artist


initialModel : Model
initialModel =
  { inputText = ""
  , artists = []
  }


-- UPDATE

type Action
  = NoOp
  | SetArtists (List Artist)
  | Create
  --| Update Int
  --| Delete Int
  | UpdateInputText String


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SetArtists artists ->
      { model | artists = artists }

    Create ->
      createArtist model

    --Update id ->
    --  model

    --Delete id ->
    --  model

    UpdateInputText txt ->
      { model | inputText = txt }


createArtist : Model -> Model
createArtist model =
  { model | artists = {id = 0, name = model.inputText} :: model.artists }


-- SIGNALS

main : Signal Html
main =
  Signal.map view state


state : Signal Model
state = Signal.foldp update initialModel actions.signal


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


get : Task Http.Error (List Artist)
get =
  Http.get decoder "/api/artists"


port runner : Task Http.Error ()
port runner =
  get `andThen` (SetArtists >> Signal.send actions.address)


-- VIEW

view : Model -> Html
view model =
  let th' field = th [] [text field]
      tr' artist = tr [] [ td [] [text <| toString artist.id]
                         , td [] [text <| artist.name]
                         , td []
                           [ button [Attr.type' "button", Attr.class "btn btn-danger"] [text "Delete"]
                           ]
                         ]
  in
    div [Attr.class "container"]

    [ entryForm model
    , table [Attr.class "table table-striped table-bordered"]
      [ thead [] [tr [] (List.map th' ["ID", "Name", "Actions"])]
      , tbody [] (List.map tr' model.artists)
      ]
    ]


entryForm : Model -> Html
entryForm model =
  div [ ]
  [ input
    [ Attr.type' "text"
    , Attr.placeholder "Artist name..."
    , Attr.value model.inputText
    , Attr.name "artist"
    , Attr.autofocus True
    , onInput actions.address UpdateInputText
    ][]
  , button [ Attr.class "add", onClick actions.address Create ] [ text "Add" ]
  , h4 [] [text (toString model)]
  ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))
