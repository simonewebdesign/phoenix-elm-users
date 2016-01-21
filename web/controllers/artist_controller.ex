defmodule MyApp.ArtistController do
  use MyApp.Web, :controller

  alias MyApp.Artist

  plug :scrub_params, "artist" when action in [:create, :update]

  def index_html(conn, _params) do
    render conn, "index.html"
  end

  def index(conn, params) do
    paginator = Artist
    |> Artist.filtered(params["name"])
    |> Repo.paginate(params)

    render conn, "index.json",
      artists: paginator.entries,
      page_number: paginator.page_number,
      page_size: paginator.page_size,
      total_pages: paginator.total_pages,
      total_entries: paginator.total_entries
  end

  def create(conn, %{"artist" => artist_params}) do
    changeset = Artist.changeset(%Artist{}, artist_params)

    case Repo.insert(changeset) do
      {:ok, artist} ->
        conn
        |> put_status(:created)
        |> put_resp_header("location", artist_path(conn, :show, artist))
        |> render("show.json", artist: artist)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(MyApp.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    artist = Repo.get!(Artist, id)
    render(conn, "show.json", artist: artist)
  end

  def update(conn, %{"id" => id, "artist" => artist_params}) do
    artist = Repo.get!(Artist, id)
    changeset = Artist.changeset(artist, artist_params)

    case Repo.update(changeset) do
      {:ok, artist} ->
        render(conn, "show.json", artist: artist)
      {:error, changeset} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render(MyApp.ChangesetView, "error.json", changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    artist = Repo.get!(Artist, id)

    # Here we use delete! (with a bang) because we expect
    # it to always work (and if it does not, it will raise).
    Repo.delete!(artist)

    send_resp(conn, :no_content, "")
  end
end
