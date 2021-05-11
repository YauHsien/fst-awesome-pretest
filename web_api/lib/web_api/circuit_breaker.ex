defmodule WebApi.CircuitBreaker do
  @moduledoc """
  A circuit breaker which break circuit when either out of connection
  capability or out of connection to supporting devices.

  Dependency:
  - WebApi.Configuration
  """
  use GenServer

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  @impl true
  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast(_msg, state) do
    {:noreply, state}
  end
end
