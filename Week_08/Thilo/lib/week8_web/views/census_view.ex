defmodule Week8Web.V1.CensusView do
  use Week8Web, :view
  alias Week8Web.V1.CensusView

  def render("index.json", %{census: census, params: params}) do
    IO.puts "bla"
    IO.inspect params
    json = %{data: render_many(census, CensusView, "census.json")}

    IO.inspect json

    if Map.has_key?(params, "status") == true do
      if params["status"] == "active" do
    json = NestedFilter.drop_by_key(json, [:inactive_no_party, :inactive_other, :inactive_total, :inactive_democrat, :inactive_republican, :inactive_libertarian])
    
      end
    end
    if Map.has_key?(params, "status") == true do
      if params["status"] == "inactive" do
    json = NestedFilter.drop_by_key(json, [:active_no_party, :active_other, :active_total, :active_democrat, :active_republican, :active_libertarian])
    
      end
    end

    if Map.has_key?(params, "party") == true do
      if params["party"] == "no_party" do
    json = NestedFilter.drop_by_key(json, [:active_other, :active_total, :active_democrat, :active_republican, :active_libertarian, :inactive_other, :inactive_total, :inactive_democrat, :inactive_republican, :inactive_libertarian])
      end
    end
    if Map.has_key?(params, "party") == true do
      if params["party"] == "other" do
    json = NestedFilter.drop_by_key(json, [:active_no_party, :active_total, :active_democrat, :active_republican, :active_libertarian, :inactive_no_party, :inactive_total, :inactive_democrat, :inactive_republican, :inactive_libertarian])
      end
    end

        if Map.has_key?(params, "party") == true do
      if params["party"] == "democrat" do
    json = NestedFilter.drop_by_key(json, [:active_no_party, :active_other, :active_total, :active_republican, :active_libertarian, :inactive_no_party, :inactive_other, :inactive_total, :inactive_republican, :inactive_libertarian])
      end
    end

        if Map.has_key?(params, "party") == true do
      if params["party"] == "republican" do
    json = NestedFilter.drop_by_key(json, [:active_no_party, :active_other, :active_total, :active_democrat, :active_libertarian, :inactive_no_party, :inactive_other, :inactive_total, :inactive_democrat, :inactive_libertarian])
      end
    end

        if Map.has_key?(params, "party") == true do
      if params["party"] == "libertarian" do
    json = NestedFilter.drop_by_key(json, [:active_no_party, :active_other, :active_total, :active_democrat, :active_republican, :inactive_no_party, :inactive_other, :inactive_total, :inactive_democrat, :inactive_republican])
      end
    end

     json
      # Enum.each(json["data"], Map.drop(json, [:inactive_no_party, :inactive_other, :inactive_total]))
    # json = Map.drop(json, [:inactive_democrat, :inactive_republican, :inactive_libertarian])
  end

  def render("show.json", %{census: census, params: params}) do
    %{data: render_one(census, CensusView, "census.json")}
  end

  def render("census.json", %{census: census}) do
    %{id: census.id,
      date: census.date,
      county: census.county,
      fips: census.fips,
      active_democrat: census.active_democrat,
      active_republican: census.active_republican,
      active_libertarian: census.active_libertarian,
      active_no_party: census.active_no_party,
      active_other: census.active_other,
      active_total: census.active_total,
      inactive_democrat: census.inactive_democrat,
      inactive_republican: census.inactive_republican,
      inactive_libertarian: census.inactive_libertarian,
      inactive_no_party: census.inactive_no_party,
      inactive_other: census.inactive_other,
      inactive_total: census.inactive_total,
      grand_total: census.grand_total,
      primary_lat: census.primary_lat,
      primary_long: census.primary_long}

  end
end
