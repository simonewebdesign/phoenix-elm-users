module Artist where

import Debug
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen, onError)
import StartApp
import Effects exposing (Effects, Never)

-- MODEL

type alias Model =
  { inputText : String
  , artists : List Artist
  , nextPage : Int
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
  , nextPage = 1
  }


-- UPDATE

type Action
  = NoOp
  --| SetArtists (List Artist)
  | AppendArtists (Maybe (List Artist))
  | SetArtist Artist
  | DeleteArtist Int
  | UpdateInputText String
  | RequestNextPage


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      noFx model

    --SetArtists artists ->
    --  noFx { model | artists = artists }

    AppendArtists maybeArtists ->
      case maybeArtists of
        Nothing ->
          noFx model
        Just artists ->
          noFx
          { model 
            | artists = model.artists ++ artists
            , nextPage = model.nextPage + 1
          }

    SetArtist artist ->
      noFx { model | artists = artist :: model.artists, inputText = "" }

    --Update id ->
    --  model

    DeleteArtist id ->
      noFx { model | artists = List.filter (\artist -> artist.id /= id ) model.artists }

    UpdateInputText txt ->
      noFx { model | inputText = txt }

    RequestNextPage ->
      (model, getArtists model.nextPage)


getArtists : Int -> Effects Action
getArtists page =
  get page
  |> Task.toMaybe
  |> Task.map AppendArtists
  |> Effects.task


-- SIGNALS

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }


init =
  (,) 
    initialModel
    (getArtists initialModel.nextPage)
    --(get 1 `andThen` (SetArtists >> Signal.send actions.address))

main : Signal Html
main =
  app.html
  --Signal.map view state


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


--state : Signal Model
--state = Signal.foldp update initialModel inputs


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


-- Refactor this: should belong to init (see Example 5 from the Elm architecture)
--port runner : Task Http.Error ()
--port runner =
  


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


-- FIXME: you probably don't need this anymore. That's why everything stopped working probably.
tasksMailbox =
  Signal.mailbox (Task.succeed ())


--port myTasks : Signal (Task Never ())
--Trying to send an unsupported type through inbound port `myTasks`

--188│ port myTasks : Signal (Task Never ())
--     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--The specific unsupported type is:

--    Task.Task Effects.Never ()

--The types of values that can flow through inbound ports include:

--    Ints, Floats, Bools, Strings, Maybes, Lists, Arrays, Tuples, Json.Values,
--    and concrete records.


port apiTasks : Signal (Task Http.Error ())
port apiTasks =
  tasksMailbox.signal


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
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


noFx : Model -> (Model, Effects Action)
noFx model =
  (,) model Effects.none


port scrolledToBottom : Signal Bool
