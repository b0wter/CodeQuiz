defmodule Week8.CenusTest do
  use Week8.DataCase

  alias Week8.Cenus

  describe "census" do
    alias Week8.Cenus.Census

    @valid_attrs %{active_democrat: 42, active_libertarian: 42, active_no_party: 42, active_other: 42, active_republican: 42, active_total: 42, date: "2010-04-17 14:00:00.000000Z", fips: 42, grand_total: 42, inactive_democrat: 42, inactive_libertarian: 42, inactive_no_party: 42, inactive_other: 42, inactive_republican: 42, inactive_total: 42, primary_lat: 120.5, primary_long: 120.5}
    @update_attrs %{active_democrat: 43, active_libertarian: 43, active_no_party: 43, active_other: 43, active_republican: 43, active_total: 43, date: "2011-05-18 15:01:01.000000Z", fips: 43, grand_total: 43, inactive_democrat: 43, inactive_libertarian: 43, inactive_no_party: 43, inactive_other: 43, inactive_republican: 43, inactive_total: 43, primary_lat: 456.7, primary_long: 456.7}
    @invalid_attrs %{active_democrat: nil, active_libertarian: nil, active_no_party: nil, active_other: nil, active_republican: nil, active_total: nil, date: nil, fips: nil, grand_total: nil, inactive_democrat: nil, inactive_libertarian: nil, inactive_no_party: nil, inactive_other: nil, inactive_republican: nil, inactive_total: nil, primary_lat: nil, primary_long: nil}

    def census_fixture(attrs \\ %{}) do
      {:ok, census} =
        attrs
        |> Enum.into(@valid_attrs)
        |> Cenus.create_census()

      census
    end

    test "list_census/0 returns all census" do
      census = census_fixture()
      assert Cenus.list_census() == [census]
    end

    test "get_census!/1 returns the census with given id" do
      census = census_fixture()
      assert Cenus.get_census!(census.id) == census
    end

    test "create_census/1 with valid data creates a census" do
      assert {:ok, %Census{} = census} = Cenus.create_census(@valid_attrs)
      assert census.active_democrat == 42
      assert census.active_libertarian == 42
      assert census.active_no_party == 42
      assert census.active_other == 42
      assert census.active_republican == 42
      assert census.active_total == 42
      assert census.date == DateTime.from_naive!(~N[2010-04-17 14:00:00.000000Z], "Etc/UTC")
      assert census.fips == 42
      assert census.grand_total == 42
      assert census.inactive_democrat == 42
      assert census.inactive_libertarian == 42
      assert census.inactive_no_party == 42
      assert census.inactive_other == 42
      assert census.inactive_republican == 42
      assert census.inactive_total == 42
      assert census.primary_lat == 120.5
      assert census.primary_long == 120.5
    end

    test "create_census/1 with invalid data returns error changeset" do
      assert {:error, %Ecto.Changeset{}} = Cenus.create_census(@invalid_attrs)
    end

    test "update_census/2 with valid data updates the census" do
      census = census_fixture()
      assert {:ok, census} = Cenus.update_census(census, @update_attrs)
      assert %Census{} = census
      assert census.active_democrat == 43
      assert census.active_libertarian == 43
      assert census.active_no_party == 43
      assert census.active_other == 43
      assert census.active_republican == 43
      assert census.active_total == 43
      assert census.date == DateTime.from_naive!(~N[2011-05-18 15:01:01.000000Z], "Etc/UTC")
      assert census.fips == 43
      assert census.grand_total == 43
      assert census.inactive_democrat == 43
      assert census.inactive_libertarian == 43
      assert census.inactive_no_party == 43
      assert census.inactive_other == 43
      assert census.inactive_republican == 43
      assert census.inactive_total == 43
      assert census.primary_lat == 456.7
      assert census.primary_long == 456.7
    end

    test "update_census/2 with invalid data returns error changeset" do
      census = census_fixture()
      assert {:error, %Ecto.Changeset{}} = Cenus.update_census(census, @invalid_attrs)
      assert census == Cenus.get_census!(census.id)
    end

    test "delete_census/1 deletes the census" do
      census = census_fixture()
      assert {:ok, %Census{}} = Cenus.delete_census(census)
      assert_raise Ecto.NoResultsError, fn -> Cenus.get_census!(census.id) end
    end

    test "change_census/1 returns a census changeset" do
      census = census_fixture()
      assert %Ecto.Changeset{} = Cenus.change_census(census)
    end
  end
end
