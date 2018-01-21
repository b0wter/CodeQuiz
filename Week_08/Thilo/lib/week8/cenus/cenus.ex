defmodule Week8.Cenus do
  @moduledoc """
  The Cenus context.
  """

  import Ecto.Query, warn: false
  alias Week8.Repo

  alias Week8.Cenus.Census

  @doc """
  Returns the list of census.

  ## Examples

      iex> list_census()
      [%Census{}, ...]

  """
  def list_census(params) do
    IO.inspect params
    filters = [] 

    if Map.has_key?(params, "county") == true do
      filters = Keyword.put(filters, :county, params["county"])
    end

    if Map.has_key?(params, "month") == true do
      filters = Keyword.put(filters, :month, params["month"])
    end


    limit = 
    if Map.has_key?(params, "limit") == true do
      case Integer.parse(params["limit"]) do
        {limit, _} -> limit 
        :error -> 500
      end
    end
  
    IO.inspect limit

    IO.inspect filters

    Census
    |> where(^filters)
    |> limit(^limit)
    |> Repo.all
  end

  @doc """
  Gets a single census.

  Raises `Ecto.NoResultsError` if the Census does not exist.

  ## Examples

      iex> get_census!(123)
      %Census{}

      iex> get_census!(456)
      ** (Ecto.NoResultsError)

  """
  def get_census!(id), do: Repo.get!(Census, id)

  @doc """
  Creates a census.

  ## Examples

      iex> create_census(%{field: value})
      {:ok, %Census{}}

      iex> create_census(%{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def create_census(attrs \\ %{}) do
    %Census{}
    |> Census.changeset(attrs)
    |> Repo.insert()
  end

  @doc """
  Updates a census.

  ## Examples

      iex> update_census(census, %{field: new_value})
      {:ok, %Census{}}

      iex> update_census(census, %{field: bad_value})
      {:error, %Ecto.Changeset{}}

  """
  def update_census(%Census{} = census, attrs) do
    census
    |> Census.changeset(attrs)
    |> Repo.update()
  end

  @doc """
  Deletes a Census.

  ## Examples

      iex> delete_census(census)
      {:ok, %Census{}}

      iex> delete_census(census)
      {:error, %Ecto.Changeset{}}

  """
  def delete_census(%Census{} = census) do
    Repo.delete(census)
  end

  @doc """
  Returns an `%Ecto.Changeset{}` for tracking census changes.

  ## Examples

      iex> change_census(census)
      %Ecto.Changeset{source: %Census{}}

  """
  def change_census(%Census{} = census) do
    Census.changeset(census, %{})
  end
end
