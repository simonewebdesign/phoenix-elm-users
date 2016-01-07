defmodule MyApp.UserView do
  use MyApp.Web, :view

  def render("index.json", %{users: users}) do
    %{data: render_many(users, MyApp.UserView, "user.json")}
  end

  def render("show.json", %{user: user}) do
    %{data: render_one(user, MyApp.UserView, "user.json")}
  end

  def render("user.json", %{user: user}) do
    %{id: user.id,
      username: user.username,
      email: user.email,
      hash: user.hash,
      recovery_hash: user.recovery_hash,
      timestamps: user.timestamps}
  end
end
