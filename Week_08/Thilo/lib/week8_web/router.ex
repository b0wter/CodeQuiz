defmodule Week8Web.Router do
  use Week8Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", Week8Web do
    pipe_through :api

    scope "/v1", V1, as: :v1 do
      resources "/census", CensusController, only: [:index, :show]
    end
  end
end
