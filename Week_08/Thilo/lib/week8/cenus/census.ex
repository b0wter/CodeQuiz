defmodule Week8.Cenus.Census do
  use Ecto.Schema
  import Ecto.Changeset
  alias Week8.Cenus.Census


  schema "census" do
    field :active_democrat, :integer, allow_nil: true
    field :active_libertarian, :integer, allow_nil: true
    field :active_no_party, :integer, allow_nil: true
    field :active_other, :integer
    field :active_republican, :integer
    field :active_total, :integer
    field :date, :utc_datetime
    field :month, :integer
    field :county, :string
    field :fips, :integer
    field :grand_total, :integer
    field :inactive_democrat, :integer
    field :inactive_libertarian, :integer
    field :inactive_no_party, :integer
    field :inactive_other, :integer
    field :inactive_republican, :integer
    field :inactive_total, :integer
    field :primary_lat, :float
    field :primary_long, :float

    timestamps()
  end

  @doc false
  def changeset(%Census{} = census, attrs) do
    census
    |> cast(attrs, [:date, :month, :county, :fips, :active_democrat, :active_republican, :active_libertarian, :active_no_party, :active_other, :active_total, :inactive_democrat, :inactive_republican, :inactive_libertarian, :inactive_no_party, :inactive_other, :inactive_total, :grand_total, :primary_lat, :primary_long])
    |> validate_required([:date, :month, :county, :fips])
  end
end
