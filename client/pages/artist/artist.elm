module Artist where

import Debug
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen, onError)


-- MODEL

type alias Model =
  { inputText : String
  , artists : List Artist
  , page : Int
  }

type alias Artist =
  { id: Int
  , name: String
  }

type alias ArtistPayload = { data: Artist }


decoder : Json.Decode.Decoder (List Artist)
decoder =
  "data" := Json.Decode.list artistDecoder


artistDecoder : Json.Decode.Decoder Artist
artistDecoder =
  Json.Decode.object2 Artist
    ("id" := Json.Decode.int)
    ("name" := Json.Decode.string)


payloadDecoder : Json.Decode.Decoder ArtistPayload
payloadDecoder =
  Json.Decode.object1 ArtistPayload ("data" := artistDecoder)


initialModel : Model
initialModel =
  { inputText = ""
  , artists = []
  , page = 1
  }


-- UPDATE

type Action
  = NoOp
  | SetArtists (List Artist)
  | AppendArtists (List Artist)
  | SetArtist Artist
  | DeleteArtist Int
  | UpdateInputText String
  | RequestNextPage


update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model

    SetArtists artists ->
      { model | artists = artists }

    AppendArtists artists ->
      { model | artists = model.artists ++ artists }

    SetArtist artist ->
      { model | artists = artist :: model.artists, inputText = "" }

    --Update id ->
    --  model

    DeleteArtist id ->
      { model | artists = List.filter (\artist -> artist.id /= id ) model.artists }

    UpdateInputText txt ->
      { model | inputText = txt }

    RequestNextPage ->
      { model | page = model.page + 1 }


-- SIGNALS

main : Signal Html
main =
  Signal.map view state


state : Signal Model
state = Signal.foldp update initialModel inputs


actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


inputs : Signal Action
inputs =
  let
    scroll = Signal.map (always RequestNextPage) scrolledToBottom
  in
    Signal.merge actions.signal scroll


get : Int -> Task Http.Error (List Artist)
get page =
  Http.get decoder ("/api/artists/?page=" ++ toString page)


port runner : Task Http.Error ()
port runner =
  get 1 `andThen` (SetArtists >> Signal.send actions.address)


postArtist : String -> Task Http.Error ()
postArtist name =
  let
    url = "http://localhost:4000/api/artists"
    body =
      Http.multipart
        [ Http.stringData "artist[name]" name ]
  in
    Http.post payloadDecoder url body
    `andThen` (\{data} -> Signal.send actions.address (SetArtist data))
    `onError` (\error -> Signal.send actions.address (SetArtist {id = -1, name = toString error}))


deleteArtist : Int -> Task Http.Error ()
deleteArtist id =
  Http.send Http.defaultSettings
    { verb = "DELETE"
    , headers = []
    , url = "http://localhost:4000/api/artists/" ++ toString id
    , body = Http.empty
    }
  `andThen` (\_ -> Signal.send actions.address (DeleteArtist id))
  `onError` (\error -> Signal.send actions.address (SetArtist {id = -2, name = toString error}))


tasksMailbox =
  Signal.mailbox (Task.succeed ())


port apiTasks : Signal (Task Http.Error ())
port apiTasks =
  tasksMailbox.signal


-- VIEW

view : Model -> Html
view model =
  let th' field = th [] [text field]
      tr' artist = tr [] [ td [] [text <| toString artist.id]
                         , td [] [text <| artist.name]
                         , td []
                           [ button [Attr.type' "button", Attr.class "btn btn-danger", onClick tasksMailbox.address (deleteArtist artist.id)] [text "Delete"]
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
  , button [ Attr.class "add", onClick tasksMailbox.address (postArtist model.inputText) ] [ text "Add" ]
  , h4 [] [text (toString model)]
  ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))


port scrolledToBottom : Signal Bool
