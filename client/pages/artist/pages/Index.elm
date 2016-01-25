module Index where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Effects exposing (Effects, Never)
import RouteHash as Router exposing (HashUpdate)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen, onError)

type alias Model =
  { inputText : String
  , searchText : String
  , artists : List Artist
  , nextPage : Int
  }

initialModel : Model
initialModel =
  { inputText = ""
  , searchText = ""
  , artists = []
  , nextPage = 1
  }

type Action 
  = NoOp
  -- GET /api/artists (index)
  --| GetArtists
  | SetArtists (Maybe (List Artist))
  -- GET /api/artists/:id (show)
  --| GetArtist Int
  | SetArtist (Maybe Artist)
  -- POST /api/artists (create)
  | CreateArtist { name : String }
  -- DELETE /api/artists/:id
  | DeleteArtist Int
  | RemoveArtist (Maybe Int)
  -- Filtering (e.g. /api/artists?name=...)
  | ReplaceArtists (Maybe (List Artist))
  | RequestNextPage
  | UpdateInputText String
  | UpdateSearchText String


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


init : ( Model, Effects Action )
init =
  ( initialModel, (getArtists initialModel.nextPage) )


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    NoOp ->
      ( model
      , Effects.none
      )

    ReplaceArtists maybeArtists ->
      case maybeArtists of
        Nothing ->
          ( model
          , Effects.none
          )

        Just [] ->
          -- Empty result set, probably because we reached the last page
          ( model
          , Effects.none
          )

        Just artists ->
          ( { model
              | artists = artists
            }
          , Effects.none
          )

    SetArtists maybeArtists ->
      case maybeArtists of
        Nothing ->
          ( model
          , Effects.none
          )

        Just [] ->
          -- Empty result set, probably because we reached the last page
          ( model
          , Effects.none
          )

        Just artists ->
          ( { model
              | artists = model.artists ++ artists
              , nextPage = model.nextPage + 1
            }
          , Effects.none
          )

    SetArtist maybeArtist ->
      case maybeArtist of
        Nothing ->
          ( model
          , Effects.none
          )

        Just artist ->
          ( { model
              | artists = artist :: model.artists
              , inputText = ""
            }
          , Effects.none
          )

    CreateArtist artist ->
      ( model
      , maybeCreateArtist artist
      )

    DeleteArtist id ->
      ( model
      , maybeDeleteArtist id
      )

    RemoveArtist maybeId ->
      case maybeId of
        Nothing ->
          ( model
          , Effects.none
          )

        Just id ->
          ( { model
              | artists = List.filter (\artist -> artist.id /= id ) model.artists
            }
          , Effects.none
          )

    UpdateInputText txt ->
      ( { model | inputText = txt }
      , Effects.none
      )

    UpdateSearchText txt ->
      ( { model | searchText = txt }
      , maybeFilterResults { name = txt }
      )

    RequestNextPage ->
      ( model
      , getArtists model.nextPage
      )


view : Signal.Address Action -> Model -> Html
view address model =
  let
    th' field =
      th [] [text field]
    tr' artist =
      tr []
      [ 
      td [] [text <| toString artist.id]
      , td [] [text <| artist.name]
      , td []
        [ button [Attr.type' "button", Attr.class "btn btn-danger", onClick address (DeleteArtist artist.id)] [text "Delete"]
        ]
      ]
  in
    div [Attr.class "container"]
    [ div []
      [ a [ Attr.href "#page-tag-1" ] [ text "go to page tag 1" ]
      , a [ Attr.href "#page-tag-2" ] [ text "go to page tag 2" ]
      ]
    , filterForm address model
    , entryForm address model
    , table [Attr.class "table table-striped table-bordered"]
      [ thead [] [tr [] (List.map th' ["ID", "Name", "Actions"])]
      , tbody [] (List.map tr' model.artists)
      ]
    ]


entryForm : Signal.Address Action -> Model -> Html
entryForm address model =
  div []
  [ input
    [ Attr.type' "text"
    , Attr.placeholder "Artist name..."
    , Attr.value model.inputText
    , Attr.name "artist"
    , Attr.autofocus True
    , onInput address UpdateInputText
    ][]
  , button [ Attr.class "add", onClick address (CreateArtist { name = model.inputText }) ] [ text "Add" ]
  , h4 [] [text (toString model)]
  ]


filterForm : Signal.Address Action -> Model -> Html
filterForm address model =
  div [ Attr.class "search-control"]
  [ input
      [ Attr.type' "text"
      , Attr.name "search"
      , Attr.value model.searchText
      , onInput address UpdateSearchText
      ][]
  , span [ Attr.class "magnifying-glass" ] [ text "🔍" ]
  ]


onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue =
    on "input" targetValue (\str -> Signal.message address (contentToValue str))


-- ROUTING

-- For delta2update, we provide our state as the value for the URL
delta2update : Model -> Model -> Maybe HashUpdate
delta2update previousState currentState =
  Just <| Router.set [toString currentState]


-- For location2action, we generate an action that will restore our state
location2action : List String -> List Action
location2action list =
  [NoOp]


-- BUSINESS LOGIC (HTTP REQUESTS)

getArtists : Int -> Effects Action
getArtists page =
  get page
  |> Task.toMaybe
  |> Task.map SetArtists
  |> Effects.task


get : Int -> Task Http.Error (List Artist)
get page =
  Http.get decoder ("/api/artists/?page=" ++ toString page)


postArtist : { a | name : String } -> Task Http.Error ArtistPayload
postArtist artist =
  let
    url = "http://localhost:4000/api/artists"
    body =
      Http.multipart
        [ Http.stringData "artist[name]" artist.name ]
  in
    Http.post payloadDecoder url body


maybeCreateArtist : { a | name : String } -> Effects Action
maybeCreateArtist artist =
  let
    extractArtistFromData {data} = data
  in
  postArtist artist
  |> Task.map extractArtistFromData
  |> Task.toMaybe
  |> Task.map SetArtist
  |> Effects.task


deleteArtist : Int -> Task Http.Error Int
deleteArtist id =
  let
    settings = Http.defaultSettings

    request =
      { verb = "DELETE"
      , headers = []
      , url = "http://localhost:4000/api/artists/" ++ toString id
      , body = Http.empty
      }

    response = Http.send settings request
  in
    Task.mapError promoteError response `andThen` handleDeleteResponse id


promoteError : Http.RawError -> Http.Error
promoteError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError


handleDeleteResponse : Int -> Http.Response -> Task Http.Error Int
handleDeleteResponse id response =
  if 200 <= response.status && response.status < 300 then

      case response.value of
        Http.Text str ->
            Task.succeed id

        _ ->
            Task.fail (Http.UnexpectedPayload "Response body is a blob, expecting a string.")

  else

      Task.fail (Http.BadResponse response.status response.statusText)


maybeDeleteArtist : Int -> Effects Action
maybeDeleteArtist id =
  deleteArtist id
  |> Task.toMaybe
  |> Task.map RemoveArtist
  |> Effects.task


getFilteredResults : { a | name : String } -> Task Http.Error (List Artist)
getFilteredResults artist =
  Http.get decoder ("/api/artists/?name=" ++ artist.name)


maybeFilterResults : { a | name : String } -> Effects Action
maybeFilterResults artist =
  getFilteredResults artist
  |> Task.toMaybe
  |> Task.map ReplaceArtists
  |> Effects.task
