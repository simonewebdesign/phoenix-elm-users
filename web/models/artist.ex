defmodule MyApp.Artist do
  use MyApp.Web, :model

  schema "artists" do
    field :name, :string

    timestamps
  end

  @required_fields ~w(name)
  @optional_fields ~w()

  @doc """
  Creates a changeset based on the `model` and `params`.

  If no params are provided, an invalid changeset is returned
  with no validation performed.
  """
  def changeset(model, params \\ :empty) do
    model
    |> cast(params, @required_fields, @optional_fields)
  end

  def ordered_by_most_recent_first(query) do
    from artist in query,
      order_by: [desc: artist.id]
  end

  def ordered_by_name(query) do
    from artist in query,
      order_by: artist.name
  end

  def filtered(query, artist_name) do
    from artist in query,
      where: like(artist.name, ^("%#{artist_name}%")),
      order_by: [desc: artist.id]
  end
end
