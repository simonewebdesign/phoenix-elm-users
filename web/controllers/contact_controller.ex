defmodule MyApp.ContactController do
  use MyApp.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
