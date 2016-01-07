defmodule MyApp.Router do
  use Addict.RoutesHelper
  use MyApp.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/" do
    pipe_through :browser # Use the default browser stack

    get "/", MyApp.PageController, :index
    get "/show", MyApp.PageController, :show
    get "/forbidden", MyApp.PageController, :forbidden

    get "/sign-in", MyApp.SignInController, :index
    get "/register", MyApp.SignUpController, :index

    get "/bingo", MyApp.BingoController, :index
    get "/blog", MyApp.BlogController, :index
    get "/style-guide", MyApp.StyleGuideController, :index
    get "/contact", MyApp.ContactController, :index
    
    resources "/api/posts", MyApp.PostController, except: [:new, :edit]
    resources "/api/artists", MyApp.ArtistController, except: [:new, :edit]
    resources "/api/phrases", MyApp.PhraseController, except: [:new, :edit]
    resources "/api/users", MyApp.UserController, except: [:new, :edit]

    addict :routes,
      logout: [path: "/logout", controller: MyApp.UserController, action: :signout],
      recover_password: "/password/recover",
      reset_password: "/password/reset"
  end

  scope "/phrases", MyApp do
    pipe_through :api

    resources "/phrases", PhrasesController
  end

end
