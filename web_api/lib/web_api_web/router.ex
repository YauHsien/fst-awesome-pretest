defmodule WebApiWeb.Router do
  use WebApiWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", WebApiWeb do
    pipe_through :api
    get "/status", CardGameController, :status
    get "/users/:timestamp/:seq/:orders_count", CardGameController, :claim_user
    get "/cards/:timestamp/:seq/:count", CardGameController, :draw_cards
    post "/orders/new", CardGameController, :place_order
    get "/trades/:card_type/:count", CardGameController, :track_trades
  end
end
