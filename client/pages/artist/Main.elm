module Artist where

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, targetValue)
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
  { currentPage : Page
  -- pages' models
  , index : Index.Model
  , pageModule1 : PageModule1.Model
  , pageModule2 : PageModule2.Model
  }

initialModel : Model
initialModel =
  { currentPage = Index
  , index = Index.initialModel
  , pageModule1 = fst PageModule1.init
  , pageModule2 = fst PageModule2.init
  }

type Page
  = Index
  | PageModule1
  | PageModule2


-- UPDATE

type Action
  = NoOp
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

    ShowPage page ->
      ( { model | currentPage = page }
      , Effects.none
      )

    IndexAction subaction ->
      let
        ( newModel, fx ) = Index.update subaction model.index
      in
        ( { model | index = newModel }
        , Effects.map IndexAction fx
        )

    PageModule1Action subaction ->
      let
        ( newModel, fx ) = PageModule1.update subaction model.pageModule1
      in
        ( { model | pageModule1 = newModel }
        , Effects.map PageModule1Action fx
        )

    PageModule2Action subaction ->
      let
        ( newModel, fx ) = PageModule2.update subaction model.pageModule2
      in
        ( { model | pageModule2 = newModel }
        , Effects.map PageModule2Action fx
        )


-- SIGNALS

app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = inputs
    }


init : ( Model, Effects Action )
init =
  let
    effects =
      Effects.batch
        [ Effects.map IndexAction <| snd Index.init
        ]
  in
    ( initialModel, effects )


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
    scroll = Signal.map (always <| IndexAction Index.RequestNextPage) scrolledToBottom
  in
    [scroll, messages.signal]


-- ROUTES

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
        [NoOp]


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
