# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Week8.Repo.insert!(%Week8.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

alias Week8.Cenus.Census
alias Week8.Repo

defmodule Week8.Seeds do

  def store_it(row) do
    IO.inspect row
    row = fix_date(row)
    row = add_month(row)
    changeset = Census.changeset(%Census{}, row)
    Repo.insert!(changeset)
  end

  def fix_date(%{date: <<m1,m0,"/",d1,d0,"/",y3,y2,y1,y0," ",h1,h0,":",mm1,mm0,":",s1,s0," AM">>} = row) do
    date = <<y3,y2,y1,y0,"-",m1,m0,"-",d1,d0,"T",h1,h0,":",mm1,mm0,":",s1,s0,"Z">>
    Map.update!(row,:date,fn _ -> date end)
  end

  def add_month(%{date: <<y3,y2,y1,y0,"-",m1,m0,"-",d1,d0,"T",h1,h0,":",mm1,mm0,":",s1,s0,"Z">>} = row) do
    month = <<m1,m0>>
    Map.put(row, :month, month)
  end


end

csv_file = Path.join(["#{:code.priv_dir(:week8)}", "repo", "State_of_Iowa_-_Monthly_Voter_Registration_Totals_by_County.csv"])

File.stream!(csv_file)
  |> Stream.drop(1)
  |> CSV.decode(headers: [:date, :fips, :county, :active_democrat, :active_republican, :active_libertarian, :active_no_party, :active_other, :active_total, :inactive_democrat, :inactive_republican, :inactive_libertarian, :inactive_no_party, :inactive_other, :inactive_total, :grand_total, :primary_lat, :primary_long])
  |> Enum.each(&Week8.Seeds.store_it/1)
