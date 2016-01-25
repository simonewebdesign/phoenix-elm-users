module Artist where

--import Debug
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode exposing ((:=))
import Task exposing (Task, andThen, onError)
import StartApp
import Effects exposing (Effects, Never)
import RouteHash as Router exposing (HashUpdate)

-- PAGES
import Index 
import PageModule1
import PageModule2

-- MODEL

type alias Model =
  { inputText : String
  , searchText : String
  , artists : List Artist
  , currentPage : Page
  , nextPage : Int
  -- pages
  , index : Index.Model
  , pageModule1 : PageModule1.Model
  , pageModule2 : PageModule2.Model
  }

type alias Artist =
  { id: Int
  , name: String
  }

type Page
  = Index
  | PageModule1
  | PageModule2

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
  , searchText = ""
  , artists = []
  , currentPage = Index
  , nextPage = 1
  , index = fst Index.init
  , pageModule1 = fst PageModule1.init
  , pageModule2 = fst PageModule2.init
  }


-- UPDATE

type Action
  = NoOp
  -- GET /api/artists (index)
  --| GetArtists
  | SetArtists (Maybe (List Artist))
  -- GET /api/artists/:id (show)
  --| GetArtist Int
  | SetArtist (Maybe (Artist))
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
  | ShowPage Page
  | IndexAction Index.Action
  | PageModule1Action PageModule1.Action
  | PageModule2Action PageModule2.Action


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

    ShowPage page ->
      ( { model | currentPage = page }
      , Effects.none
      )

    IndexAction subaction ->
      let
        (newModel, fx) = Index.update subaction model.index
      in
        ( { model | index = newModel }
        , Effects.map IndexAction fx
        )

    PageModule1Action subaction ->
      let
        (newModel, fx) = PageModule1.update subaction model.pageModule1
      in
        ( { model | pageModule1 = newModel }
        , Effects.map PageModule1Action fx
        )

    PageModule2Action subaction ->
      let
        (newModel, fx) = PageModule2.update subaction model.pageModule2
      in
        ( { model | pageModule2 = newModel }
        , Effects.map PageModule2Action fx
        )

    --IndexAction subaction ->
    --  ( { model | index = Index.update subaction model.index }
    --  , Effects.none
    --  )

    --PageModule1Action subaction ->
    --  ( { model | pageModule1 = PageModule1.update subaction model.pageModule1 }
    --  , Effects.none
    --  )

    --PageModule2Action subaction ->
    --  ( { model | pageModule2 = PageModule2.update subaction model.pageModule2 }
    --  , Effects.none
    --  )



getArtists : Int -> Effects Action
getArtists page =
  get page
  |> Task.toMaybe
  |> Task.map SetArtists
  |> Effects.task


-- SIGNALS

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }

init : (Model, Effects Action)
init =
  (,) initialModel (getArtists initialModel.nextPage)


main : Signal Html
main =
  app.html


port tasks : Signal (Task Never ())
port tasks =
  app.tasks


port routeTasks : Signal (Task () ())
port routeTasks =
  Router.start
      { prefix = Router.defaultPrefix
      , models = app.model
      , delta2update = delta2update
      , address = messages.address
      , location2action = location2action
      }


{- In your `Main` module, create a mailbox for your action type ... something
like this. Of course, the exact details depend on your `Action` type. Note that
you'll typically need to define a `NoOp` action in order to fulfill the
requirement for signals to have an initial value.
-}
messages : Signal.Mailbox Action
messages =
    Signal.mailbox NoOp


inputs : List (Signal Action)
inputs =
  let
    scroll = Signal.map (always RequestNextPage) scrolledToBottom
  in
    [scroll, messages.signal]


-- ROUTES


-- Routing

-- So, the main thing we'll do here to start with is modify the hash to
-- indicate which example we're currently looking at. Note that we don't have
-- to check whether it has changed, because the elm-route-hash module will
-- check for that. So, in this case, we don't care about the previous value.
-- And, we can always return a HashUpdate, since it will only actually be
-- set when it changes.
delta2update : Model -> Model -> Maybe HashUpdate
delta2update prev current =
    case current.currentPage of
      Index ->
        -- First, we ask the submodule for a HashUpdate. Then, we use
        -- `map` to prepend something to the URL.
        Router.map ((::) "listing") <|
          Index.delta2update prev.index current.index

      PageModule1 ->
          Router.map ((::) "page-tag-1") <|
            PageModule1.delta2update prev.pageModule1 current.pageModule1

      PageModule2 ->
          Router.map ((::) "page-tag-2") <|
            PageModule2.delta2update prev.pageModule2 current.pageModule2


-- Here, we basically do the reverse of what delta2update does
location2action : List String -> List Action
location2action list =
    case list of
      -- We give the Index module a chance to interpret the rest of
      -- the URL, and then we prepend an action for the part we
      -- interpreted.
      "listing" :: rest ->
        ( ShowPage Index ) :: List.map IndexAction ( Index.location2action rest )
      
      "page-tag-1" :: rest ->
        ( ShowPage PageModule1 ) :: List.map PageModule1Action ( PageModule1.location2action rest )
        
      "page-tag-2" :: rest ->
        ( ShowPage PageModule2 ) :: List.map PageModule2Action ( PageModule2.location2action rest )

      _ ->
          [(UpdateInputText "showing the 404 error page")]

      --first :: rest ->
      --    case first of
      --        "page-tag-1" ->
      --            List.map ShowPage (PageModule1.location2action rest)

      --        "page-tag-2" ->
      --            List.map ShowPage (PageModule2.location2action rest)


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


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  case model.currentPage of
    Index ->
      Index.view (Signal.forwardTo address IndexAction) model.index

    PageModule1 ->
      PageModule1.view (Signal.forwardTo address PageModule1Action) model.pageModule1

    PageModule2 ->
      PageModule2.view (Signal.forwardTo address PageModule2Action) model.pageModule2


port scrolledToBottom : Signal Bool
