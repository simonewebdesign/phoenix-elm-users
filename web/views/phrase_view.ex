defmodule MyApp.PhraseView do
  use MyApp.Web, :view

  def render("index.json", %{phrases: phrases}) do
    %{data: render_many(phrases, MyApp.PhraseView, "phrase.json")}
  end

  def render("show.json", %{phrase: phrase}) do
    %{data: render_one(phrase, MyApp.PhraseView, "phrase.json")}
  end

  def render("phrase.json", %{phrase: phrase}) do
    %{id: phrase.id,
      phrase: phrase.phrase,
      points: phrase.points,
      wasSpoken: phrase.wasSpoken}
  end
end
