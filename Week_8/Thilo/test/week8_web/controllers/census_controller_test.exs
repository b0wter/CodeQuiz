defmodule Week8Web.CensusControllerTest do
  use Week8Web.ConnCase

  alias Week8.Cenus
  alias Week8.Cenus.Census

  @create_attrs %{active_democrat: 42, active_libertarian: 42, active_no_party: 42, active_other: 42, active_republican: 42, active_total: 42, date: "2010-04-17 14:00:00.000000Z", fips: 42, grand_total: 42, inactive_democrat: 42, inactive_libertarian: 42, inactive_no_party: 42, inactive_other: 42, inactive_republican: 42, inactive_total: 42, primary_lat: 120.5, primary_long: 120.5}
  @update_attrs %{active_democrat: 43, active_libertarian: 43, active_no_party: 43, active_other: 43, active_republican: 43, active_total: 43, date: "2011-05-18 15:01:01.000000Z", fips: 43, grand_total: 43, inactive_democrat: 43, inactive_libertarian: 43, inactive_no_party: 43, inactive_other: 43, inactive_republican: 43, inactive_total: 43, primary_lat: 456.7, primary_long: 456.7}
  @invalid_attrs %{active_democrat: nil, active_libertarian: nil, active_no_party: nil, active_other: nil, active_republican: nil, active_total: nil, date: nil, fips: nil, grand_total: nil, inactive_democrat: nil, inactive_libertarian: nil, inactive_no_party: nil, inactive_other: nil, inactive_republican: nil, inactive_total: nil, primary_lat: nil, primary_long: nil}

  def fixture(:census) do
    {:ok, census} = Cenus.create_census(@create_attrs)
    census
  end

  setup %{conn: conn} do
    {:ok, conn: put_req_header(conn, "accept", "application/json")}
  end

  describe "index" do
    test "lists all census", %{conn: conn} do
      conn = get conn, census_path(conn, :index)
      assert json_response(conn, 200)["data"] == []
    end
  end

  describe "create census" do
    test "renders census when data is valid", %{conn: conn} do
      conn = post conn, census_path(conn, :create), census: @create_attrs
      assert %{"id" => id} = json_response(conn, 201)["data"]

      conn = get conn, census_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "active_democrat" => 42,
        "active_libertarian" => 42,
        "active_no_party" => 42,
        "active_other" => 42,
        "active_republican" => 42,
        "active_total" => 42,
        "date" => "2010-04-17 14:00:00.000000Z",
        "fips" => 42,
        "grand_total" => 42,
        "inactive_democrat" => 42,
        "inactive_libertarian" => 42,
        "inactive_no_party" => 42,
        "inactive_other" => 42,
        "inactive_republican" => 42,
        "inactive_total" => 42,
        "primary_lat" => 120.5,
        "primary_long" => 120.5}
    end

    test "renders errors when data is invalid", %{conn: conn} do
      conn = post conn, census_path(conn, :create), census: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "update census" do
    setup [:create_census]

    test "renders census when data is valid", %{conn: conn, census: %Census{id: id} = census} do
      conn = put conn, census_path(conn, :update, census), census: @update_attrs
      assert %{"id" => ^id} = json_response(conn, 200)["data"]

      conn = get conn, census_path(conn, :show, id)
      assert json_response(conn, 200)["data"] == %{
        "id" => id,
        "active_democrat" => 43,
        "active_libertarian" => 43,
        "active_no_party" => 43,
        "active_other" => 43,
        "active_republican" => 43,
        "active_total" => 43,
        "date" => "2011-05-18 15:01:01.000000Z",
        "fips" => 43,
        "grand_total" => 43,
        "inactive_democrat" => 43,
        "inactive_libertarian" => 43,
        "inactive_no_party" => 43,
        "inactive_other" => 43,
        "inactive_republican" => 43,
        "inactive_total" => 43,
        "primary_lat" => 456.7,
        "primary_long" => 456.7}
    end

    test "renders errors when data is invalid", %{conn: conn, census: census} do
      conn = put conn, census_path(conn, :update, census), census: @invalid_attrs
      assert json_response(conn, 422)["errors"] != %{}
    end
  end

  describe "delete census" do
    setup [:create_census]

    test "deletes chosen census", %{conn: conn, census: census} do
      conn = delete conn, census_path(conn, :delete, census)
      assert response(conn, 204)
      assert_error_sent 404, fn ->
        get conn, census_path(conn, :show, census)
      end
    end
  end

  defp create_census(_) do
    census = fixture(:census)
    {:ok, census: census}
  end
end
