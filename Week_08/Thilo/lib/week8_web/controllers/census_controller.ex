defmodule Week8Web.V1.CensusController do
  use Week8Web, :controller

  alias Week8.Cenus

  action_fallback Week8Web.FallbackController

  def index(conn, params) do
    IO.inspect params
    census = Cenus.list_census(params)
    render(conn, "index.json", %{census: census, params: params})
  end

  # def create(conn, %{"census" => census_params}) do
  #   with {:ok, %Census{} = census} <- Cenus.create_census(census_params) do
  #     conn
  #     |> put_status(:created)
  #     |> put_resp_header("location", census_path(conn, :show, census))
  #     |> render("show.json", census: census)
  #   end
  # end
  #
  # def show(conn, %{"id" => id}) do
  #   census = Cenus.get_census!(id)
  #   render(conn, "show.json", census: census)
  # end
  #
  # def update(conn, %{"id" => id, "census" => census_params}) do
  #   census = Cenus.get_census!(id)
  #
  #   with {:ok, %Census{} = census} <- Cenus.update_census(census, census_params) do
  #     render(conn, "show.json", census: census)
  #   end
  # end
  #
  # def delete(conn, %{"id" => id}) do
  #   census = Cenus.get_census!(id)
  #   with {:ok, %Census{}} <- Cenus.delete_census(census) do
  #     send_resp(conn, :no_content, "")
  #   end
  # end
end
