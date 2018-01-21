defmodule Week8.Repo.Migrations.CreateCensus do
  use Ecto.Migration

  def change do
    create table(:census) do
      add :date, :utc_datetime
      add :month, :integer
      add :county, :string
      add :fips, :integer
      add :active_democrat, :integer
      add :active_republican, :integer
      add :active_libertarian, :integer
      add :active_no_party, :integer
      add :active_other, :integer
      add :active_total, :integer
      add :inactive_democrat, :integer
      add :inactive_republican, :integer
      add :inactive_libertarian, :integer
      add :inactive_no_party, :integer
      add :inactive_other, :integer
      add :inactive_total, :integer
      add :grand_total, :integer
      add :primary_lat, :float
      add :primary_long, :float

      timestamps()
    end

  end
end
