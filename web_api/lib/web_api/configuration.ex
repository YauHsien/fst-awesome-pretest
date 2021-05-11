defmodule WebApi.Configuration do
  @moduledoc """
  Configuration for the API module.

  Load presidence:
  1. Several environment variables
  2. Configuration file located at /app/config/web_api.conf
  3. Failed
  """
  use GenServer

  @impl true
  def init(_args) do
    state =
      %{
        #:user_node_name =>
        #System.get_env("UserNodeName")
        #"",
        #:trade_node_name => ""
      }
    {:ok, state}
  end
end
