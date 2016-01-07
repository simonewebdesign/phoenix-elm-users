defmodule MyApp.PhraseControllerTest do
  use MyApp.ConnCase

  alias MyApp.Phrase
  @valid_attrs %{phrase: "some content", points: 42, wasSpoken: true}
  @invalid_attrs %{}

  setup do
    conn = conn() |> put_req_header("accept", "application/json")
    {:ok, conn: conn}
  end

  test "lists all entries on index", %{conn: conn} do
    conn = get conn, phrase_path(conn, :index)
    assert json_response(conn, 200)["data"] == []
  end

  test "shows chosen resource", %{conn: conn} do
    phrase = Repo.insert! %Phrase{}
    conn = get conn, phrase_path(conn, :show, phrase)
    assert json_response(conn, 200)["data"] == %{"id" => phrase.id,
      "phrase" => phrase.phrase,
      "points" => phrase.points,
      "wasSpoken" => phrase.wasSpoken}
  end

  test "does not show resource and instead throw error when id is nonexistent", %{conn: conn} do
    assert_raise Ecto.NoResultsError, fn ->
      get conn, phrase_path(conn, :show, -1)
    end
  end

  test "creates and renders resource when data is valid", %{conn: conn} do
    conn = post conn, phrase_path(conn, :create), phrase: @valid_attrs
    assert json_response(conn, 201)["data"]["id"]
    assert Repo.get_by(Phrase, @valid_attrs)
  end

  test "does not create resource and renders errors when data is invalid", %{conn: conn} do
    conn = post conn, phrase_path(conn, :create), phrase: @invalid_attrs
    assert json_response(conn, 422)["errors"] != %{}
  end

  test "updates and renders chosen resource when data is valid", %{conn: conn} do
    phrase = Repo.insert! %Phrase{}
    conn = put conn, phrase_path(conn, :update, phrase), phrase: @valid_attrs
    assert json_response(conn, 200)["data"]["id"]
    assert Repo.get_by(Phrase, @valid_attrs)
  end

  test "does not update chosen resource and renders errors when data is invalid", %{conn: conn} do
    phrase = Repo.insert! %Phrase{}
    conn = put conn, phrase_path(conn, :update, phrase), phrase: @invalid_attrs
    assert json_response(conn, 422)["errors"] != %{}
  end

  test "deletes chosen resource", %{conn: conn} do
    phrase = Repo.insert! %Phrase{}
    conn = delete conn, phrase_path(conn, :delete, phrase)
    assert response(conn, 204)
    refute Repo.get(Phrase, phrase.id)
  end
end
