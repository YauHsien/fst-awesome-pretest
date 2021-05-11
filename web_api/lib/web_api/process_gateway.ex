defmodule WebApi.ProcessGateway do
  @moduledoc """
  Help keeping track of API call & process.

  Features:
  - Circuit breaker
  - Asynchronous results
  """
  use GenServer

  @impl true
  def init(keywords) do
    state =
      %{
        :circuit_break_threshold => Keyword.get(keywords, :conns_limit),
        :conns_count => 0
      }
    {:ok, state}
  end

  @impl true
  def handle_call({:query,:circuit_broken}, _from, state) do
    gb =
      case {Map.get(state,:conns_count), Map.get(state,:circuit_break_threshold)} do
        {c,t} when c < t ->
          :good
        _ ->
          :bad
      end
    {:reply, gb, state}
  end
  def handle_call(_request, _from, state) do
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast(_msg, state) do
    {:noreply, state}
  end
end
