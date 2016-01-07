defmodule MyApp.PostView do
  use MyApp.Web, :view

  def render("index.json", %{posts: posts}) do
    %{data: render_many(posts, MyApp.PostView, "post.json")}
  end

  def render("show.json", %{post: post}) do
    %{data: render_one(post, MyApp.PostView, "post.json")}
  end

  def render("post.json", %{post: post}) do
    %{id: post.id,
      name: post.name}
  end
end
